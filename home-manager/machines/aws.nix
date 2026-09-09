{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    awscli2
  ];

  my.secrets = {
    enable = true;
  };

}
