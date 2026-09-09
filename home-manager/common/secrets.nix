{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.secrets;

  # Wrapper that reads the decrypted NUT password and invokes nut-to-mqtt.
  nut-to-mqtt-wrapped = pkgs.writeShellApplication {
    name = "nut-to-mqtt-wrapped";
    runtimeInputs = [ pkgs.coreutils ];
    text = ''
      PASSWORD="$(cat "${config.sops.secrets.nut-password.path}")"
      exec "${config.home.homeDirectory}/.local/bin/nut-to-mqtt" \
        -mqtt_clientid="" \
        -mqtt_endpoint=tcp://mqtt:1883/ \
        -nut_username=upsmon \
        -nut_password="$PASSWORD"
    '';
  };
in
{
  options.my.secrets = {
    enable = mkEnableOption "sops-nix secret management on this machine";

    nut-password = {
      enable = mkEnableOption "NUT UPS MQTT password";
    };

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

    (mkIf cfg.nut-password.enable {
      sops.secrets.nut-password = {
        sopsFile = ../secrets/nut-password.sops.yaml;
        path = "${config.home.homeDirectory}/.config/sops-nix/secrets/nut-password";
      };

      home.packages = optionals pkgs.stdenv.isLinux [ nut-to-mqtt-wrapped ];
    })

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
