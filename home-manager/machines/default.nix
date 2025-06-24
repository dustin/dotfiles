{ config, pkgs, ... }:

{
  imports = [
    ../common/shared.nix
  ];

  home.packages = with pkgs; [
  ];

  programs.zsh.initContent = ''
  '';
}
