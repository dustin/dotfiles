# machines/dsstudio.nix
{ config, pkgs-old, pkgs, lib, hostname, ... }:

{
  imports = [
    ../common/shared.nix
    ../common/darwin.nix
  ];

  home = {
    homeDirectory = "/Users/dustin";
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
        PATH = lib.makeBinPath [ pkgs.duckdb ];
        HOME = config.home.homeDirectory;
        LANG = "en_US.UTF-8";
      };

      WorkingDirectory = "${config.home.homeDirectory}/stuff/duck";
    };
  };

  programs.zsh.initContent = lib.mkMerge [
    ''
    PATH=$PATH:$HOME/local.bin:$HOME/bin:$HOME/.local/bin:$HOME/go/bin:$PATH
    # export NIX_SSL_CERT_FILE=/Users/dustin/stuff/cert.pem
    export EDITOR=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
    ''
  ];
}
