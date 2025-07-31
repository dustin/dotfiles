{ config, pkgs, ... }:

{
  imports = [
    ../common/shared.nix
  ];

  home.packages = with pkgs; [
  ];

  progarms.zsh.oh-my-zsh = {
    enable = true;
    plugins = [ "fzf" ];
  };
