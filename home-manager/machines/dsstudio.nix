# machines/dsstudio.nix
{ config, pkgs-old, pkgs, lib, hostname, ... }:

let
  dwpMatrixToken = builtins.pathExists ../secrets/dwp-matrix-token.sops.yaml;
in
{
  home = {
    homeDirectory = "/Users/dustin";
  };

  my.secrets = {
    enable = true;
  };

  services.metubeTranscode.worker.enable = true;

  services.ninerouter.enable = true;
  services.ninerouter.host = "0.0.0.0";
  services.ninerouter.rev = "1101453fcd813cae68f0716011422a4edc3d33fa";

  programs.headroom = {
    enable = true;
    proxy = {
      enable = true;
      targetApiUrl = "http://localhost:11434/v1";
      apiKey = "ollama";
      mode = "token";
      disableKompress = true;
      port = 8787;
    };
  };

  # downwind.pro Matrix bot: GPX files sent to the room become runs.
  # Waits for its token: `sops edit secrets/dwp-matrix-token.sops.yaml`
  # (key dwp-matrix-token), and it starts on the next switch.
  sops.secrets.dwp-matrix-token = lib.mkIf dwpMatrixToken {
    sopsFile = ../secrets/dwp-matrix-token.sops.yaml;
  };
  services.dwp-matrix-bot = {
    enable = dwpMatrixToken;
    dbDir = "${config.home.homeDirectory}/prog/downwind.pro/db";
    homeserver = "sallings.org";
    room = "!ovDBT5ES62J937qC7V:sallings.org";
    users."@dustin:sallings.org" = "dustin";
    admins = [ "@dustin:sallings.org" ];
    tokenFile = config.sops.secrets.dwp-matrix-token.path;
  };

  home.packages = with pkgs; [
    pkgs-old.haskellPackages.net-mqtt # my mqtt-watch command
    # darcs
	# pgcli
  ];

  launchd.agents.duckupdate = {
    enable = true;
    config = {
      Label = "net.spy.duckupdate";

      ProgramArguments = [
        "${config.home.homeDirectory}/stuff/duck/update-all.sh"
      ];

      RunAtLoad = false;
      StartInterval = 86400;
      KeepAlive = false;

      StandardOutPath  = "${config.xdg.stateHome}/duckupdate/stdout.log";
      StandardErrorPath = "${config.xdg.stateHome}/duckupdate/stderr.log";

      EnvironmentVariables = {
        PATH = lib.makeBinPath [ pkgs.pueue pkgs.duckdb ];
        HOME = config.home.homeDirectory;
        LANG = "en_US.UTF-8";
        SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        NIX_SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
      };

      WorkingDirectory = "${config.home.homeDirectory}/stuff/duck";
    };
  };

  launchd.agents.updatebuoys = {
    enable = true;
    config = {
      Label = "net.spy.buoyupdate";

      ProgramArguments = [
        "${config.home.homeDirectory}/stuff/duck/scripts/update-buoys.sh"
      ];

      RunAtLoad = false;
      StartCalendarInterval = [
        { Minute = 10; }
        { Minute = 40; }
      ];
      KeepAlive = false;

      StandardOutPath  = "${config.xdg.stateHome}/buoyupdate/stdout.log";
      StandardErrorPath = "${config.xdg.stateHome}/buoyupdate/stderr.log";

      EnvironmentVariables = {
        PATH = lib.makeBinPath [ pkgs.duckdb pkgs.rclone ];
        HOME = config.home.homeDirectory;
        LANG = "en_US.UTF-8";
        SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        NIX_SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
      };

      WorkingDirectory = "${config.home.homeDirectory}/stuff/duck";
    };
  };
}
