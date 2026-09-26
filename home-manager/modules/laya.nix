{ config, lib, pkgs, ... }:

# https://huggingface.co/convaiinnovations/laya

let
  cfg = config.services.laya;
  laya = pkgs.callPackage ../pkgs/laya.nix { };

  modelsEnv = lib.concatStringsSep "," cfg.models;

  baseEnv = {
    LAYA_HOST = cfg.host;
    LAYA_PORT = toString cfg.port;
    LAYA_DEVICE = cfg.device;
    LAYA_PRELOAD = if cfg.preload then "1" else "0";
    LAYA_MODELS = modelsEnv;
    LAYA_AUTO_TASK = if cfg.autoTask then "1" else "0";
    UV_TOOL_DIR = "${config.home.homeDirectory}/.local/share/laya";
    UV_TOOL_BIN_DIR = "${config.home.homeDirectory}/.local/share/laya/bin";
    SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    NIX_SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  }
  // lib.optionalAttrs (cfg.threads != null) { LAYA_THREADS = toString cfg.threads; }
  // cfg.extraEnv;
in
{
  options.services.laya = {
    enable = lib.mkEnableOption "Laya System 1 decision model HTTP server (laya-serve)";

    package = lib.mkOption {
      type = lib.types.package;
      default = laya;
      defaultText = lib.literalExpression "pkgs.callPackage ../pkgs/laya.nix { }";
      description = "The laya-serve package to install.";
    };

    host = lib.mkOption {
      type = lib.types.str;
      default = "0.0.0.0";
      description = "Address laya-serve binds to.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 8400;
      description = "Port laya-serve listens on.";
    };

    device = lib.mkOption {
      type = lib.types.str;
      default = "cpu";
      description = "Torch device passed to laya-serve, e.g. \"cpu\", \"mps\", or \"cuda\".";
    };

    preload = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Load all configured checkpoints at startup instead of lazily.";
    };

    models = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ "english" ];
      description = "Checkpoints to preload/serve, e.g. \"english\", \"multilingual\", \"typed-decisions\".";
    };

    autoTask = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable Laya's automatic task detection.";
    };

    threads = lib.mkOption {
      type = lib.types.nullOr lib.types.ints.positive;
      default = null;
      description = "Cap torch intra-op threads for CPU inference (LAYA_THREADS). Keep at or below physical core count.";
    };

    apiKeyFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = ''
        Path to a file (e.g. an sops secret) containing the bearer token clients must send
        as `Authorization: Bearer <key>`. When null, the server requires no authentication.
      '';
    };

    extraEnv = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      description = "Additional environment variables for the laya-serve process.";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      home.packages = [ cfg.package ];

      home.activation.laya-setup = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -p "${config.home.homeDirectory}/.local/share/laya"
      '';
    }

    (lib.mkIf pkgs.stdenv.isDarwin {
      launchd.agents.laya-serve = {
        enable = true;
        config = {
          Label = "net.spy.laya-serve";
          ProgramArguments = lib.optionals (cfg.apiKeyFile != null) [
            "${pkgs.writeShellApplication {
              name = "laya-serve-wrapped";
              runtimeInputs = [ cfg.package pkgs.coreutils ];
              text = ''
                LAYA_API_KEY="$(cat "${toString cfg.apiKeyFile}")"
                export LAYA_API_KEY
                exec laya-serve
              '';
            }}/bin/laya-serve-wrapped"
          ] ++ lib.optionals (cfg.apiKeyFile == null) [ "${cfg.package}/bin/laya-serve" ];
          EnvironmentVariables = baseEnv;
          KeepAlive = true;
          RunAtLoad = true;
          StandardOutPath = "${config.xdg.stateHome}/laya-serve/stdout.log";
          StandardErrorPath = "${config.xdg.stateHome}/laya-serve/stderr.log";
        };
      };
    })

    (lib.mkIf pkgs.stdenv.isLinux {
      systemd.user.services.laya-serve = {
        Unit = {
          Description = "Laya System 1 decision model HTTP server";
          After = [ "network.target" ];
        };
        Service = {
          Type = "simple";
          ExecStart =
            if cfg.apiKeyFile != null then
              "${pkgs.writeShellApplication {
                name = "laya-serve-wrapped";
                runtimeInputs = [ cfg.package pkgs.coreutils ];
                text = ''
                  LAYA_API_KEY="$(cat "${toString cfg.apiKeyFile}")"
                  export LAYA_API_KEY
                  exec laya-serve
                '';
              }}/bin/laya-serve-wrapped"
            else
              "${cfg.package}/bin/laya-serve";
          Environment = lib.mapAttrsToList (k: v: "${k}=${v}") baseEnv;
          Restart = "always";
          StartLimitInterval = 0;
          RestartSec = 10;
        };
        Install.WantedBy = [ "default.target" ];
      };
    })
  ]);
}
