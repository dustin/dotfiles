{ config, pkgs, ... }:

{
  imports = [
    ../common/shared.nix
  ];

  home = {
    homeDirectory = "/Users/dustin";
  };

  home.packages = with pkgs; [
  ];

  programs.zsh.initExtra = ''
    PATH=$PATH:$HOME/.nix-profile/bin:$HOME/local.bin:$HOME/bin:$HOME/.local/bin:$HOME/go/bin:$HOME/prog/eprojects/go/bin:$PATH

    export NIX_SSL_CERT_FILE=/Users/dustin/stuff/cert.pem

    export EDITOR=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
  '';
}
