{ pkgs, bluebox }:

let
  postgresql = pkgs.postgresql_17.withPackages (ps: [
    ps.pgjwt
    ps.postgis
  ]);

  # Common environment setup
  pgEnvSetup = ''
    export PGDATA=$PWD/database/pgdata
    export PGHOST=$PWD/database/pgdata
    export PGDATABASE=example
    # Second Unix socket dir, in addition to $PGHOST, so sandboxed agent
    # tooling (seatbelt denies outbound unix-sockets by default, except a
    # /private/tmp/eae-* prefix reserved for local Postgres/PostgREST dev
    # services) can reach Postgres without opening a TCP port.
    export PGSANDBOXSOCK=/tmp/eae-postgrest-admin-preview
  '';

  # Common helper functions
  pgHelpers = ''
    # Check if PostgreSQL is running
    pg_is_running() {
      ${postgresql}/bin/pg_ctl -D "$PGDATA" status > /dev/null 2>&1
    }

    # Start PostgreSQL
    pg_start() {
      # -h ''' (empty listen_addresses) disables the TCP listener entirely;
      # Postgres is reachable only via Unix socket, either $PGHOST (used by
      # psql/load-sql/run-postgrest) or $PGSANDBOXSOCK (a /tmp/eae-* path
      # pre-allowed for sandboxed agent tooling; see pgEnvSetup above).
      mkdir -p "$PGSANDBOXSOCK"
      ${postgresql}/bin/pg_ctl \
        -D "$PGDATA" \
        -o "-c unix_socket_directories='$PGHOST,$PGSANDBOXSOCK' -h '''" \
        -l "$PGDATA/postgresql.log" \
        start
      sleep 3
    }

    # Stop PostgreSQL
    pg_stop() {
      ${postgresql}/bin/pg_ctl -D "$PGDATA" stop
    }

    # Get PostgreSQL PID
    pg_get_pid() {
      ${postgresql}/bin/pg_ctl -D "$PGDATA" status | \
        grep -o 'PID: [0-9]*' | cut -d' ' -f2
    }
  '';

  # Helper script to load all SQL files
  load-sql = pkgs.writeShellScriptBin "load-sql" ''
    set -xeuo pipefail
    ${pgEnvSetup}
    ${postgresql}/bin/psql --host="$PGHOST" -v ON_ERROR_STOP=1 -d example -f "${bluebox.schema}"

    TMPDIR=$(mktemp -d)
    ${pkgs.unzip}/bin/unzip -q "${bluebox.data}" -d "$TMPDIR"
    ${postgresql}/bin/psql --host="$PGHOST" -v ON_ERROR_STOP=1 -d example -f "$TMPDIR/bluebox_dataonly_v0.4.sql"
    rm -rf "$TMPDIR"

    ${postgresql}/bin/psql --host="$PGHOST" -d example -f "$PWD/database/roles.sql"
    ${postgresql}/bin/psql --host="$PGHOST" -v ON_ERROR_STOP=1 -d example -f "$PWD/database/permissions.sql"
  '';

  setup = pkgs.writeShellScriptBin "setup" ''
    echo "Setting up database..."

    ${pgEnvSetup}
    ${pgHelpers}

    # Initialize PostgreSQL if it doesn't exist
    if [ ! -f "$PGDATA/PG_VERSION" ]; then
      echo "Initializing PostgreSQL database..."
      mkdir -p "$PGDATA"
      ${postgresql}/bin/initdb \
        -D "$PGDATA" \
        --auth-local=trust \
        --auth-host=trust
    else
      echo "PostgreSQL data directory already exists"
    fi

    # Start PostgreSQL temporarily if not running
    STARTED_POSTGRES=false
    if ! pg_is_running; then
      echo "Starting PostgreSQL temporarily for setup..."
      pg_start
      STARTED_POSTGRES=true
    fi

    # Create database if it doesn't exist
    if ! ${postgresql}/bin/psql \
         --host="$PGHOST" \
         -d postgres \
         -lqt | cut -d \| -f 1 | grep -qw example; then
      echo "Creating example database..."
      ${postgresql}/bin/createdb example --host="$PGHOST"
    else
      echo "Database 'example' already exists"
    fi

    # Load all SQL files (BlueBox + custom schema)
    load-sql

    # Stop PostgreSQL if we started it
    if [ "$STARTED_POSTGRES" = true ]; then
      echo "Stopping temporary PostgreSQL instance..."
      pg_stop
    fi

    echo ""
    echo "Setup completed successfully!"
    echo "Run 'run' to start the services"
  '';

  run-postgres = pkgs.writeShellScriptBin "run-postgres" ''
    echo "Starting PostgreSQL..."

    ${pgEnvSetup}
    ${pgHelpers}

    # Check if setup has been run
    if [ ! -f "$PGDATA/PG_VERSION" ]; then
      echo "Error: PostgreSQL not initialized. Run 'setup' first."
      exit 1
    fi

    # Start PostgreSQL if not already running
    if pg_is_running; then
      echo "PostgreSQL is already running"
      POSTGRES_PID=$(pg_get_pid)
    else
      echo "Starting PostgreSQL..."
      pg_start
      POSTGRES_PID=$(pg_get_pid)

      # Start PostgreSQL log tailer
      echo "Starting PostgreSQL log monitor..."
      tail -f "$PGDATA/postgresql.log" 2>/dev/null &
      LOG_TAILER_PID=$!
    fi

    echo "PostgreSQL service started successfully"
    echo "Database: example"
    echo "Socket: $PGHOST/.s.PGSQL.5432"
    echo "Connection: postgres:///?host=$PGHOST&dbname=example"
    echo "PostgreSQL PID: $POSTGRES_PID"

    # Trap to cleanup PostgreSQL on exit
    cleanup() {
      echo "Stopping PostgreSQL..."
      if [ -n "$LOG_TAILER_PID" ] && kill -0 "$LOG_TAILER_PID" 2>/dev/null; then
        kill "$LOG_TAILER_PID" 2>/dev/null
      fi
      if [ -d "$PGDATA" ] && pg_is_running; then
        pg_stop
      fi
      exit
    }

    trap cleanup INT TERM

    # Keep the script running
    if [ -n "$LOG_TAILER_PID" ]; then
      wait $LOG_TAILER_PID
    else
      while kill -0 "$POSTGRES_PID" 2>/dev/null; do
        sleep 1
      done
    fi
  '';

  load-dump = pkgs.writeShellScriptBin "load-dump" ''
    ${pgEnvSetup}
    ${pgHelpers}

    # Check if database is initialized
    if [ ! -f "$PGDATA/PG_VERSION" ]; then
      echo "Error: Database not initialized. Run 'setup' first."
      exit 1
    fi

    # Start PostgreSQL temporarily if not running
    STARTED_POSTGRES=false
    if ! pg_is_running; then
      echo "PostgreSQL is not running, starting it temporarily..."
      pg_start
      STARTED_POSTGRES=true
    fi

    # Load all SQL files (BlueBox + custom schema)
    load-sql

    # Stop PostgreSQL if we started it
    if [ "$STARTED_POSTGRES" = true ]; then
      echo "Stopping temporary PostgreSQL instance..."
      pg_stop
    fi
  '';

  database = pkgs.writeShellScriptBin "database" ''
    ${pgEnvSetup}

    echo "Connecting to database: example"
    exec ${postgresql}/bin/psql --host="$PGHOST" -d example
  '';

in
{
  inherit postgresql;
  scripts = [ setup run-postgres load-sql load-dump database ];
  buildInputs = [ pkgs.unzip postgresql ];
}
