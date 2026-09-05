# machines/dsmac.nix
{ config, pkgs-old, pkgs, lib, hostname, ... }:

{
  home = {
    homeDirectory = "/Users/dustin";
  };

  home.packages = with pkgs; [
    pkgs-old.haskellPackages.net-mqtt # my mqtt-watch command
    darcs
	  # pgcli
  ];

  programs.zsh.initContent = lib.mkMerge [
    ''
    export PATH="$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$HOME/local.bin:$HOME/bin:$HOME/.local/bin:$HOME/go/bin:$PATH"
    # export NIX_SSL_CERT_FILE=/Users/dustin/stuff/cert.pem
    export EDITOR=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
    ''
  ];
}
