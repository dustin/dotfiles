# machines/dsmac.nix
{ config, pkgs-old, pkgs, lib, hostname, ... }:

{
  home = {
    homeDirectory = "/Users/dustin";
  };

  my.secrets = {
    enable = true;
    rclone-config = {
      enable = true;
      remotes = [ "drive" "dropbox" "gphoto" "s3" ];
    };
  };

  home.packages = with pkgs; [
    pkgs-old.haskellPackages.net-mqtt # my mqtt-watch command
    darcs
	  # pgcli
  ];
}
