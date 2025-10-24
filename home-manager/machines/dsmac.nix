# machines/dsmac.nix
{ config, pkgs-old, pkgs, lib, hostname, ... }:

{
  imports = [
    ../common/shared.nix
  ];

  home = {
    homeDirectory = "/Users/dustin";
  };

  home.packages = with pkgs; [
    pkgs-old.haskellPackages.net-mqtt # my mqtt-watch command
    darcs
	  # pgcli
  ];

  launchd.agents.pueue = {
    enable = true;
    config = {
      Label = "net.spy.pueued";
      ProgramArguments = [ "${pkgs.pueue}/bin/pueued" ];
      RunAtLoad = true;

      # Relaunch on exit or crash
      KeepAlive = true;

      StandardOutPath = "${config.xdg.stateHome}/my-server/stdout.log";
      StandardErrorPath = "${config.xdg.stateHome}/my-server/stderr.log";

      EnvironmentVariables = {
        PATH = lib.makeBinPath [ pkgs.pueue ];
        HOME = config.home.homeDirectory;
        LANG = "en_US.UTF-8";
      };
      WorkingDirectory = config.home.homeDirectory;
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
