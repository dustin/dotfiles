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
    rclone-config = {
      enable = true;
      remotes = [ "drive" "dropbox" "gphoto" "s3" ];
      # The dwp bot finds riders' Drive folders by remote name and
      # publishes to s3; walter is only for it.
      extra.dwp-bot.remotes = [ "walter" "s3" ];
    };
  };

  services.metubeTranscode.worker.enable = true;

  # zfs-sync-to-ducklake: per-host ZFS manifests into the DuckLake,
  # daily at 05:00 from ~/stuff/duckdb (where init.sql lives).
  programs.zfsDucklakeSync.enable = true;

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
    checkout = "${config.home.homeDirectory}/prog/downwind.pro";
    homeserver = "sallings.org";
    room = "!ovDBT5ES62J937qC7V:sallings.org";
    users."@dustin:sallings.org" = "dustin";
    tokenFile = config.sops.secrets.dwp-matrix-token.path;
    # The launchd agent can't make LAN connections (macOS Local Network
    # privacy); bee1 resolves over Tailscale.
    mqtt = "bee1";
  };
  launchd.agents.dwp-matrix-bot.config.EnvironmentVariables.RCLONE_CONFIG =
    config.my.secrets.rclone-config.extra.dwp-bot.path;
  # Wailea cam water motion every 10 min in daylight, to
  # weather/conditions/wailea_motion; skipped while Kihei is under 9 kn.
  services.dwp-wailea-motion = {
    enable = true;
    mqtt = "bee1";
  };

  home.packages = with pkgs; [
    pkgs-old.haskellPackages.net-mqtt # my mqtt-watch command
    # darcs
	# pgcli
  ];

  launchd.agents.duckbackup = {
    enable = true;
    config = {
      Label = "net.spy.duckbackup";

      ProgramArguments = [
        "${config.home.homeDirectory}/stuff/duck/back-the-duck-up.sh"
      ];

      RunAtLoad = false;
      StartCalendarInterval = [
        {
          Hour = 3;
          Minute = 0;
        }
      ];
      KeepAlive = false;

      StandardOutPath  = "${config.xdg.stateHome}/duckbackup/stdout.log";
      StandardErrorPath = "${config.xdg.stateHome}/duckbackup/stderr.log";

      EnvironmentVariables = {
        PATH = lib.makeBinPath [ pkgs.pueue pkgs.duckdb pkgs.xz pkgs.rclone pkgs.coreutils ];
        HOME = config.home.homeDirectory;
        LANG = "en_US.UTF-8";
        SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        NIX_SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
      };

      WorkingDirectory = "${config.home.homeDirectory}/stuff/duck";
    };
  };

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
