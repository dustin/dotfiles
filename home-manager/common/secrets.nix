{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.secrets;
in
{
  options.my.secrets = {
    enable = mkEnableOption "sops-nix secret management on this machine";

    aws-credentials = {
      enable = mkEnableOption "AWS credentials file";
    };

    rclone-config = {
      enable = mkEnableOption "rclone config file";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # sops CLI for editing secrets.
      home.packages = [ pkgs.sops ];

      # Use the existing age key location from the agenix attempt.
      sops.age.keyFile = "${config.home.homeDirectory}/.config/age/keys.txt";

      # Common secrets: installed by default on any machine with secrets enabled.
      my.secrets.aws-credentials.enable = mkDefault true;
      my.secrets.rclone-config.enable = mkDefault true;
    }

    (mkIf cfg.aws-credentials.enable {
      sops.secrets.aws-credentials = {
        sopsFile = ../secrets/aws-credentials.sops.yaml;
        path = "${config.home.homeDirectory}/.aws/credentials";
      };

      home.file = {
        ".aws/.keep".text = "";
      };
    })

    (mkIf cfg.rclone-config.enable {
      sops.secrets.rclone-config = {
        sopsFile = ../secrets/rclone-config.sops.yaml;
        path = "${config.home.homeDirectory}/.config/rclone/rclone.conf";
      };

      home.file = {
        ".config/rclone/.keep".text = "";
      };
    })
  ]);
}
