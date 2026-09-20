{ config, lib, pkgs, ... }:

let
  cfg = config.services.bambuWeightFetcher;
  fetcher = pkgs.callPackage ../pkgs/bambu-weight-fetcher.nix { };
in
{
  options.services.bambuWeightFetcher = {
    enable = lib.mkEnableOption "Bambu printer weight-fetching HTTP service";

    printerHost = lib.mkOption {
      type = lib.types.str;
      default = "a1mini.lan";
      description = "Hostname or IP of the Bambu printer.";
    };

    listenHost = lib.mkOption {
      type = lib.types.str;
      default = "0.0.0.0";
      description = "Address the HTTP server binds to.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 8788;
      description = "Port the HTTP server listens on.";
    };
  };

  config = lib.mkIf cfg.enable {
    sops.secrets.bambu-access-code = {
      sopsFile = ../secrets/bambu-access-code.sops.yaml;
      path = "${config.home.homeDirectory}/.config/sops-nix/secrets/bambu-access-code";
    };

    systemd.user.services.bambu-weight-fetcher = lib.mkIf pkgs.stdenv.isLinux {
      Unit = {
        Description = "Bambu printer weight fetcher";
        After = [ "network.target" ];
      };
      Service = {
        Type = "simple";
        ExecStart = "${pkgs.writeShellApplication {
          name = "bambu-weight-fetcher-run";
          runtimeInputs = [ fetcher pkgs.coreutils ];
          text = ''
            export BAMBU_PRINTER_HOST="${cfg.printerHost}"
            export BAMBU_LISTEN_HOST="${cfg.listenHost}"
            export BAMBU_LISTEN_PORT="${toString cfg.port}"
            BAMBU_ACCESS_CODE="$(cat "${config.sops.secrets.bambu-access-code.path}")"
            export BAMBU_ACCESS_CODE
            exec bambu-weight-fetcher
          '';
        }}/bin/bambu-weight-fetcher-run";
        Restart = "always";
        StartLimitInterval = 0;
        RestartSec = 10;
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
}
