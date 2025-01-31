{ systemd, config, pkgs, ... }:

{
  imports = [
    ../common/shared.nix
    ../common/linux.nix
  ];

  home.packages = with pkgs; [
    awscli2
  ];

}
