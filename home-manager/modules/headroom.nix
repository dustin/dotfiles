{ config, lib, pkgs, ... }:

let
  cfg = config.programs.headroom;
  headroom = pkgs.callPackage ../pkgs/headroom.nix { };
  headroom-proxy = pkgs.callPackage ../pkgs/headroom-proxy.nix { inherit headroom; };
in
{
  options.programs.headroom = {
    enable = lib.mkEnableOption "Headroom proxy tooling";

    package = lib.mkOption {
      type = lib.types.package;
      default = headroom;
      defaultText = lib.literalExpression "pkgs.callPackage ../pkgs/headroom.nix { }";
      description = "The headroom package to install.";
    };

    proxy = {
      enable = lib.mkEnableOption "Headroom proxy launchd service";

      targetApiUrl = lib.mkOption {
        type = lib.types.str;
        default = "http://localhost:11434/v1";
        description = "Upstream API URL the proxy forwards to (e.g. Ollama).";
      };

      apiKey = lib.mkOption {
        type = lib.types.str;
        default = "ollama";
        description = "API key presented to the upstream endpoint.";
      };

      mode = lib.mkOption {
        type = lib.types.str;
        default = "token";
        description = "Headroom processing mode.";
      };

      disableKompress = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether to disable headroom compression.";
      };

      port = lib.mkOption {
        type = lib.types.port;
        default = 8787;
        description = "Local port the proxy listens on.";
      };
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      home.packages = [ cfg.package headroom-proxy ];
    }

    (lib.mkIf cfg.proxy.enable {
      launchd.agents.headroom-proxy = {
        enable = true;
        config = {
          Label = "org.headroom.proxy";
          ProgramArguments = [
            "${cfg.package}/bin/headroom"
            "proxy"
            "--port"
            (toString cfg.proxy.port)
          ];
          EnvironmentVariables = {
            OPENAI_TARGET_API_URL = cfg.proxy.targetApiUrl;
            OPENAI_API_KEY = cfg.proxy.apiKey;
            HEADROOM_MODE = cfg.proxy.mode;
            HEADROOM_DISABLE_KOMPRESS = if cfg.proxy.disableKompress then "1" else "0";
            UV_TOOL_DIR = "${config.home.homeDirectory}/.local/share/headroom";
            UV_TOOL_BIN_DIR = "${config.home.homeDirectory}/.local/share/headroom/bin";
          };
          KeepAlive = true;
          RunAtLoad = true;
          StandardOutPath = "${config.home.homeDirectory}/.local/share/headroom/headroom-proxy.log";
          StandardErrorPath = "${config.home.homeDirectory}/.local/share/headroom/headroom-proxy.log";
        };
      };

      home.activation.headroom-setup = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -p "${config.home.homeDirectory}/.local/share/headroom"
      '';
    })
  ]);
}
