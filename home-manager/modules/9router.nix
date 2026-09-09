{ config, lib, pkgs, ... }:

let
  cfg = config.services.ninerouter;

  ninerouter = pkgs.writeShellScriptBin "9router" ''
    set -e

    export PORT="${toString cfg.port}"
    export HOSTNAME="${cfg.host}"
    export DATA_DIR="${cfg.dataDir}"
    export NEXT_PUBLIC_BASE_URL="${cfg.nextPublicBaseUrl}"
    export BASE_URL="${cfg.baseUrl}"
    export REQUIRE_API_KEY="${if cfg.requireApiKey then "true" else "false"}"
    export INITIAL_PASSWORD="${cfg.initialPassword}"
    export HOME="${config.home.homeDirectory}"

    cd "${cfg.sourceDir}"
    exec ${cfg.package}/bin/node "${cfg.sourceDir}/custom-server.js" \
      --port ${toString cfg.port} \
      --hostname ${cfg.host}
  '';
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
            PATH = lib.makeBinPath [ cfg.package ];
            HOME = config.home.homeDirectory;
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
            PATH = lib.makeBinPath [ cfg.package ];
            HOME = config.home.homeDirectory;
          } // cfg.extraEnv);
        };
        Install.WantedBy = [ "default.target" ];
      };
    })

    {
      home.activation.ninerouter-setup = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -p "${config.xdg.stateHome}/9router" "${cfg.dataDir}"
      '';
    }
  ]);
}
