{ config, pkgs, lib, ... }:

{
  imports = [
    ./common/shared.nix
  ];
  
  home = {
    username = "dustin";
    homeDirectory = "/Users/dustin";
    stateVersion = "23.11";
  };
  
  # Common configuration...
  home.packages = with pkgs; [
    haskellPackages.net-mqtt
    darcs
  ];
  
  # You can still use conditionals based on hostname if needed
  programs.zsh.initExtra = lib.mkMerge [
    ''
      PATH=$PATH:$HOME/local.bin:$HOME/bin:$HOME/.local/bin:$HOME/go/bin:$PATH
      export EDITOR=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
    ''
    (lib.mkIf (config.networking.hostName == "laptop") ''
      # Some laptop-specific zsh config
    '')
  ];
  
  programs.home-manager.enable = true;
}
