{ config, lib, pkgs, ... }:

let
  cfg = config.services.ninerouter;

  # Tools the launcher script and npm need at runtime (npm spawns `sh`).
  servicePkgs = [ cfg.package pkgs.git pkgs.coreutils pkgs.bash ];

  ninerouter = pkgs.writeShellApplication {
    name = "9router";
    runtimeInputs = servicePkgs ++ [ pkgs.cacert ];
    text = ''
      set -e

      export SSL_CERT_FILE="${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
      export NIX_SSL_CERT_FILE="${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
      export PORT="${toString cfg.port}"
      export HOSTNAME="${cfg.host}"
      export DATA_DIR="${cfg.dataDir}"
      export NEXT_PUBLIC_BASE_URL="${cfg.nextPublicBaseUrl}"
      export BASE_URL="${cfg.baseUrl}"
      export REQUIRE_API_KEY="${if cfg.requireApiKey then "true" else "false"}"
      export INITIAL_PASSWORD="${cfg.initialPassword}"
      export HOME="${config.home.homeDirectory}"

      SRC_DIR="${cfg.sourceDir}"
      STATE_DIR="${config.xdg.stateHome}/9router"
      LOG_FILE="$STATE_DIR/update.log"
      BUILT_REV_FILE="$STATE_DIR/built-rev"
      mkdir -p "$STATE_DIR"

      echo "=== 9router startup $(date) ===" >> "$LOG_FILE"

      NEEDS_BUILD=0

      if [ ! -d "$SRC_DIR/.git" ]; then
        echo "Cloning ${cfg.sourceRepo} into $SRC_DIR" >> "$LOG_FILE"
        mkdir -p "$(dirname "$SRC_DIR")"
        git clone --depth 1 "${cfg.sourceRepo}" "$SRC_DIR" >> "$LOG_FILE" 2>&1
        NEEDS_BUILD=1
      ${lib.optionalString cfg.autoUpdate ''
      else
        echo "Pulling latest changes" >> "$LOG_FILE"
        git -C "$SRC_DIR" pull --ff-only >> "$LOG_FILE" 2>&1 || echo "git pull failed, continuing with existing checkout" >> "$LOG_FILE"
      ''}
      fi

      CUR_REV="$(git -C "$SRC_DIR" rev-parse HEAD)"
      BUILT_REV="$(cat "$BUILT_REV_FILE" 2>/dev/null || true)"
      if [ "$BUILT_REV" != "$CUR_REV" ]; then
        echo "HEAD $CUR_REV differs from last built ''${BUILT_REV:-none}, rebuilding" >> "$LOG_FILE"
        NEEDS_BUILD=1
      fi

      if [ ! -d "$SRC_DIR/node_modules" ] || [ "$NEEDS_BUILD" = "1" ]; then
        echo "Running npm install" >> "$LOG_FILE"
        (cd "$SRC_DIR" && npm install >> "$LOG_FILE" 2>&1)
      fi

      if [ ! -d "$SRC_DIR/.next" ] || [ "$NEEDS_BUILD" = "1" ]; then
        echo "Running npm run build" >> "$LOG_FILE"
        (cd "$SRC_DIR" && npm run build >> "$LOG_FILE" 2>&1)
      fi

      # Only reached if install/build above succeeded (set -e); records
      # the rev so a failed build retries on the next restart instead of
      # silently launching a stale .next.
      echo "$CUR_REV" > "$BUILT_REV_FILE"

      cd "$SRC_DIR"
      exec node "$SRC_DIR/custom-server.js" \
        --port ${toString cfg.port} \
        --hostname ${cfg.host}
    '';
  };
in
{
  options.services.ninerouter = {
    enable = lib.mkEnableOption "9router AI proxy/router";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.nodejs;
      description = "Node.js package used to run 9router.";
    };

    sourceDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/.local/share/9router";
      description = "Path to the built 9router source tree.";
    };

    sourceRepo = lib.mkOption {
      type = lib.types.str;
      default = "https://github.com/decolua/9router.git";
      description = "Git repository to clone 9router from when sourceDir isn't already set up.";
    };

    autoUpdate = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Pull the latest 9router source (and rebuild if it changed) every time the service starts.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 20128;
      description = "Port 9router listens on.";
    };

    host = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = "Host 9router binds to.";
    };

    dataDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/.9router";
      description = "Directory for 9router data and SQLite database.";
    };

    baseUrl = lib.mkOption {
      type = lib.types.str;
      default = "http://${cfg.host}:${toString cfg.port}";
      description = "Server-side base URL used by 9router.";
    };

    nextPublicBaseUrl = lib.mkOption {
      type = lib.types.str;
      default = "http://${cfg.host}:${toString cfg.port}";
      description = "Public base URL used by the dashboard.";
    };

    requireApiKey = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Require a Bearer API key on /v1/* routes.";
    };

    initialPassword = lib.mkOption {
      type = lib.types.str;
      default = "changeme";
      description = "Initial dashboard login password.";
    };

    extraEnv = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = {};
      description = "Additional environment variables for the 9router process.";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      home.packages = [ cfg.package ninerouter ];
    }

    (lib.mkIf pkgs.stdenv.isDarwin {
      launchd.agents.ninerouter = {
        enable = true;
        config = {
          Label = "org.ninerouter";
          ProgramArguments = [ "${ninerouter}/bin/9router" ];
          KeepAlive = true;
          RunAtLoad = true;
          WorkingDirectory = cfg.sourceDir;
          EnvironmentVariables = {
            PATH = lib.makeBinPath servicePkgs;
            HOME = config.home.homeDirectory;
            SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
            NIX_SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
          } // cfg.extraEnv;
          StandardOutPath = "${config.xdg.stateHome}/9router/stdout.log";
          StandardErrorPath = "${config.xdg.stateHome}/9router/stderr.log";
        };
      };
    })

    (lib.mkIf pkgs.stdenv.isLinux {
      systemd.user.services.ninerouter = {
        Unit.Description = "9router AI proxy/router";
        Service = {
          Type = "simple";
          ExecStart = "${ninerouter}/bin/9router";
          WorkingDirectory = cfg.sourceDir;
          Restart = "on-failure";
          Environment = lib.mapAttrsToList (k: v: "${k}=${v}") ({
            PATH = lib.makeBinPath servicePkgs;
            HOME = config.home.homeDirectory;
            SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
            NIX_SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
          } // cfg.extraEnv);
        };
        Install.WantedBy = [ "default.target" ];
      };
    })

    {
      # Actual clone/update/build happens in the launcher script on every
      # service start (see `ninerouter` above); activation just ensures
      # the directories it needs already exist.
      home.activation.ninerouter-setup = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -p "${config.xdg.stateHome}/9router" "${cfg.dataDir}"
      '';
    }
  ]);
}
