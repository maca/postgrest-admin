{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.pga;
  dataDir = "/var/lib/pga";
  postgrestSocket = "/run/pga/pga.sock";
  serviceName = "pga";
  staticAssets = import ./static-assets.nix { inherit pkgs; };
  bluebox = import ./bluebox.nix { inherit pkgs; };

  # Fetch pre-built Elm application from GitHub releases
  # NB: pin an explicit release tag, not /releases/latest, so builds are reproducible.
  elmApp = pkgs.stdenv.mkDerivation {
    pname = "pga-elm";
    version = "build-11";

    src = pkgs.fetchurl {
      url = "https://github.com/maca/postgrest-admin/releases/download/build-11/postgrest-admin.min.js";
      sha256 = "sha256-ZJWbLRN1SaG8yErG5oCki9DyqXA2Udcj9RJNoHbd4sI=";
    };

    dontUnpack = true;

    installPhase = ''
      mkdir -p $out
      cp $src $out/postgrest-admin.min.js
    '';
  };


  loginBannerText = ''
    **Welcome to postgrest-admin demo!**

    An on-the-fly automated back-office/admin interface builder for PostgREST
    api's, with minimalist configuration.

    Login with user: **bluebox@example.com** and password: **password**.

    postgrest-admin source is hosted at:
    [https://github.com/maca/postgrest-admin](https://github.com/maca/postgrest-admin)

    The data used in this demo is a 3rd party simulation of a back-office
    database for a DVD Rental chain, available here:
    [https://github.com/ryanbooz/bluebox](https://github.com/ryanbooz/bluebox)
  '';

  # Package all static assets together
  staticBundle = pkgs.runCommand "pga-static" { } ''
        mkdir -p $out
        cp ${elmApp}/postgrest-admin.min.js $out/postgrest-admin.min.js
        cp ${staticAssets.redoc}/redoc.standalone.js $out/redoc.standalone.js
        cp ${staticAssets.icono}/icono.min.css $out/icono.min.css
        cp ${staticAssets.milligram}/milligram.min.css $out/milligram.min.css
        cp ${../static/app.css} $out/app.css

        cat > $out/index.html <<'EOF'
    <!DOCTYPE HTML>
    <html>
    <head>
      <meta charset="UTF-8">
      <title>Main</title>
      <style>body { padding: 0; margin: 0; }</style>
      <link rel="stylesheet" href="/icono.min.css" />
      <link rel="stylesheet" href="/milligram.min.css" />
      <link rel="stylesheet" href="/app.css" type="text/css" media="screen" />
      <script src="/postgrest-admin.min.js"></script>
    </head>

    <body>
        <script type="text/javascript">
            console.log("token: ", sessionStorage.getItem("jwt"))
            const app = Elm.Main.init({
                flags: {
                    jwt: sessionStorage.getItem("jwt"),
                    host: "https://pga.bitmunge.com/api",
                    loginUrl: "https://pga.bitmunge.com/api/rpc/login",
                    clientHeaders: {
                      "Accept-Profile": "bluebox",
                      "Content-Profile": "bluebox"
                    },
                    "loginBannerText": ${builtins.toJSON loginBannerText}
                }
            })

            app.ports.loggedIn.subscribe(jwt => {
                console.log("got token: ", jwt)
                sessionStorage.setItem("jwt", jwt)
            });

            app.ports.loggedOut.subscribe(_ => {
                sessionStorage.removeItem("jwt")
            });
        </script>
    <body>
    EOF
  '';


  # PostgREST config file
  postgrestConf = pkgs.writeText "postgrest.conf" ''
    db-uri = "postgres:///?host=${cfg.databaseSocket}&dbname=${serviceName}"
    db-schemas = "public, bluebox"
    db-anon-role = "web_anon"
    jwt-secret = "DL+P8+muauKgOSqRKqIKMkjcUpLZ5ajXScgA965i/Bg="
    server-unix-socket = "${postgrestSocket}"
    log-level = "info"
  '';
in
{
  options.services.pga = {
    enable = mkEnableOption "PostgREST Admin (PGA) service";

    databaseSocket = mkOption {
      type = types.str;
      default = "/run/postgresql";
      description = "PostgreSQL unix socket directory";
    };

    accessLog = mkOption {
      type = types.str;
      default = "/var/log/nginx/pga.access.log";
      description = "Access log path for the PGA virtual host";
    };
  };

  config = mkIf cfg.enable {
    users.users.pga = {
      isNormalUser = true;
      # isSystemUser = true;
      group = "web";
      home = dataDir;
      createHome = true;
      description = "PostgREST Admin service user";
    };
    users.groups.web = { };


    systemd.services.pga = {
      description = "PostgREST Admin (PGA)";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" "postgresql.service" "pga-db-load.service" ];
      requires = [ "pga-db-load.service" ];

      serviceConfig = {
        Type = "simple";
        User = serviceName;
        Group = "web";

        ExecStart = "${pkgs.postgrest}/bin/postgrest ${postgrestConf}";

        Restart = "on-failure";
        RestartSec = "5s";

        # Security
        PrivateTmp = true;
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ReadOnlyPaths = [ dataDir ];
        RuntimeDirectory = serviceName;
        RuntimeDirectoryMode = "0755";
      };
    };


    services.postgresql = {
      package = pkgs.postgresql_17;
      enable = true;
      enableTCPIP = false;
      extensions = with pkgs.postgresql_17.pkgs; [ postgis pgjwt ];
      ensureUsers = [
        { name = serviceName; }
        {
          name = "authenticator";
          ensureClauses = {
            login = false;
            "inherit" = false;
          };
        }
        {
          name = "web_anon";
          ensureClauses = { login = false; };
        }
        {
          name = "bluebox";
          ensureClauses = { login = false; };
        }
      ];
    };


    systemd.services.pga-db-create = {
      description = "Create PGA database and extensions";
      after = [ "postgresql.service" ];
      requires = [ "postgresql.service" ];

      serviceConfig = {
        Type = "oneshot";
        User = "postgres";
        Group = "postgres";
      };

      script = ''
        set -xeuo pipefail
        echo "Creating PGA database..."
        ${config.services.postgresql.package}/bin/psql -d postgres -c "DROP DATABASE IF EXISTS ${serviceName};"
        ${config.services.postgresql.package}/bin/psql -d postgres -c "CREATE DATABASE ${serviceName} OWNER ${serviceName};"

        echo "Creating extensions..."
        ${config.services.postgresql.package}/bin/psql -d ${serviceName} -c "CREATE EXTENSION IF NOT EXISTS pgcrypto CASCADE;"
        ${config.services.postgresql.package}/bin/psql -d ${serviceName} -c "CREATE EXTENSION IF NOT EXISTS pgjwt CASCADE;"
        ${config.services.postgresql.package}/bin/psql -d ${serviceName} -c "CREATE EXTENSION IF NOT EXISTS postgis CASCADE;"
      '';
    };


    systemd.services.pga-db-load = {
      description = "Load PGA database schema and data";
      after = [ "postgresql.service" "pga-db-create.service" ];
      requires = [ "pga-db-create.service" ];

      serviceConfig = {
        Type = "oneshot";
        User = serviceName;
        Group = "web";
      };

      script = ''
        set -xeuo pipefail
        echo "Loading PGA database schema and data..."
        ${config.services.postgresql.package}/bin/psql -v ON_ERROR_STOP=1 -d ${serviceName} -f ${bluebox.schema}

        TMPDIR=$(mktemp -d)
        ${pkgs.unzip}/bin/unzip -q ${bluebox.data} -d "$TMPDIR"
        ${config.services.postgresql.package}/bin/psql -v ON_ERROR_STOP=1 -d ${serviceName} -f "$TMPDIR/bluebox_dataonly_v0.4.sql"
        rm -rf "$TMPDIR"

        ${config.services.postgresql.package}/bin/psql -v ON_ERROR_STOP=1 -d ${serviceName} -f ${../database/permissions.sql}
        echo "Database load completed successfully"
      '';
    };


    services.nginx = {
      enable = true;

      virtualHosts."pga.bitmunge.com" = {
        root = staticBundle;
        forceSSL = true;
        enableACME = true;

        extraConfig = ''
          access_log ${cfg.accessLog};
        '';

        locations."/" = {
          tryFiles = "$uri @fallback";
        };

        locations."/api/" = {
          proxyPass = "http://unix:${postgrestSocket}:/";
          extraConfig = ''
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
          '';
        };

        locations."@fallback" = {
          extraConfig = ''
            # Only serve index.html if the request has no file extension
            if ($uri !~ \.[a-zA-Z0-9]+$) {
              rewrite ^ /index.html break;
            }
            return 404;
          '';
        };
      };
    };

    users.users.nginx.extraGroups = [ "web" "acme" ];
    networking.firewall.allowedTCPPorts = [ 80 443 ];

    security.acme = {
      acceptTerms = mkDefault true;
      defaults.email = mkDefault "pga@macario.fastmail.com";
      certs."pga.bitmunge.com" = {
        webroot = mkDefault "/var/lib/acme/acme-challenge";
        group = "acme";
      };
    };
  };
}
