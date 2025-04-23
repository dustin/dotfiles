# machines/dsmac.nix
{ config, pkgs, lib, hostname, ... }:

{
  imports = [
    ../common/shared.nix
  ];

  home = {
    homeDirectory = "/Users/dustin";
  };

  home.packages = with pkgs; [
    haskellPackages.net-mqtt # my mqtt-watch command
    darcs
	pgcli
  ];

  programs.zsh.initExtra = lib.mkMerge [
    ''
    PATH=$PATH:$HOME/local.bin:$HOME/bin:$HOME/.local/bin:$HOME/go/bin:$PATH
    # export NIX_SSL_CERT_FILE=/Users/dustin/stuff/cert.pem
    export EDITOR=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
    ''
  ];
}
