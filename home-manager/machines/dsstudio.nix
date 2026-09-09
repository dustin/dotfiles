# machines/dsstudio.nix
{ config, pkgs-old, pkgs, lib, hostname, ... }:

{
  home = {
    homeDirectory = "/Users/dustin";
  };

  my.secrets = {
    enable = true;
  };

  services.ninerouter.enable = true;

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
      StartInterval = 3600;
      KeepAlive = false;

      StandardOutPath  = "${config.xdg.stateHome}/buoyupdate/stdout.log";
      StandardErrorPath = "${config.xdg.stateHome}/buoyupdate/stderr.log";

      EnvironmentVariables = {
        PATH = lib.makeBinPath [ pkgs.duckdb pkgs.rclone ];
        HOME = config.home.homeDirectory;
        LANG = "en_US.UTF-8";
      };

      WorkingDirectory = "${config.home.homeDirectory}/stuff/duck";
    };
  };
}
