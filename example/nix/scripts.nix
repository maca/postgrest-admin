{ pkgs, postgresql }:

let
  run = pkgs.writeShellScriptBin "run" ''
    PORT=''${1:-9000}

    echo "Starting all services..."

    # Run setup if the database hasn't been initialized yet (setup's
    # load-sql step is not safe to re-run against an existing database)
    if [ ! -f "$PWD/database/pgdata/PG_VERSION" ]; then
      echo "Database not initialized, running setup..."
      setup
    fi

    # Initialize service PIDs
    PIDS=""

    # Start PostgreSQL in background
    echo "Starting PostgreSQL service..."
    { run-postgres 2>&1 1>&3 | sed 's/.*/\x1b[31mPOSTGRES: &\x1b[0m/' >&2; } 3>&1 | sed 's/.*/\x1b[34mPOSTGRES: &\x1b[0m/' &
    POSTGRES_PID=$!
    PIDS="$PIDS $POSTGRES_PID"

    # Give PostgreSQL time to start
    sleep 5

    # Start PostgREST in background
    echo "Starting PostgREST service..."
    { run-postgrest 2>&1 1>&3 | sed 's/.*/\x1b[31mPGREST: &\x1b[0m/' >&2; } 3>&1 | sed 's/.*/\x1b[33mPGREST: &\x1b[0m/' &
    POSTGREST_PID=$!
    PIDS="$PIDS $POSTGREST_PID"
    sleep 2

    # Start elm-watch in background
    echo "Starting elm-watch service..."
    { elm-watch hot 2>&1 1>&3 | sed 's/.*/\x1b[31mELM-WATCH: &\x1b[0m/' >&2; } 3>&1 | sed 's/.*/\x1b[32mELM-WATCH: &\x1b[0m/' &
    ELM_WATCH_PID=$!
    PIDS="$PIDS $ELM_WATCH_PID"
    sleep 2

    # Trap to cleanup all services on exit
    cleanup() {
      echo ""
      echo "Stopping all services..."
      for pid in $PIDS; do
        if kill -0 "$pid" 2>/dev/null; then
          echo "Stopping process $pid"
          kill "$pid" 2>/dev/null
        fi
      done

      # Wait a moment for graceful shutdown
      sleep 2

      # Force kill any remaining processes
      for pid in $PIDS; do
        if kill -0 "$pid" 2>/dev/null; then
          echo "Force stopping process $pid"
          kill -9 "$pid" 2>/dev/null
        fi
      done

      exit
    }

    trap cleanup INT TERM

    echo ""
    echo "All services started successfully!"
    echo "- PostgreSQL: Database server running on unix socket"
    echo "- PostgREST: API server on http://localhost:9080"
    echo "- elm-watch: Elm compiler with hot reload"
    echo "- Development server: http://localhost:$PORT"
    echo "Services running with PIDs: $PIDS"
    echo "Press Ctrl+C to stop all services."

    # Wait for all background processes
    wait
  '';

  stop = pkgs.writeShellScriptBin "stop" ''
    echo "Stopping all services..."

    export PGDATA=$PWD/database/pgdata

    # Stop PostgreSQL
    if [ -d "$PGDATA" ] && \
       ${postgresql}/bin/pg_ctl -D "$PGDATA" status > /dev/null 2>&1; then
      echo "Stopping PostgreSQL..."
      ${postgresql}/bin/pg_ctl -D "$PGDATA" stop
      echo "PostgreSQL stopped successfully"
    else
      echo "PostgreSQL is not running"
    fi

    # Stop PostgREST if running
    if pgrep -f "postgrest.*postgrest.conf" > /dev/null; then
      echo "Stopping PostgREST..."
      pkill -f "postgrest.*postgrest.conf"
      rm -f /tmp/postgrest-example.sock
      echo "PostgREST stopped successfully"
    else
      echo "PostgREST is not running"
    fi

    if pgrep -f "elm-watch" > /dev/null; then
      echo "Stopping elm-watch..."
      pkill -f "elm-watch"
      echo "elm-watch stopped successfully"
    else
      echo "elm-watch is not running"
    fi

    echo "All services stopped"
  '';

  clean = pkgs.writeShellScriptBin "clean" ''
    echo "Cleaning database..."

    export PGDATA=$PWD/database/pgdata

    # Stop services first
    stop

    # Remove PostgreSQL data directory
    if [ -d "$PGDATA" ]; then
      echo "Removing PostgreSQL data directory..."
      rm -rf "$PGDATA"
      echo "Database cleaned successfully"
    else
      echo "Database directory does not exist"
    fi

    echo ""
    echo "Run 'setup' to reinitialize the database"
  '';

in
{
  scripts = [ run stop clean ];
}
