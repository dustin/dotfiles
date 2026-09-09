{ config, lib, pkgs, ... }:

let
  cfg = config.services.nutToMqtt;

  # Wrapper that reads the decrypted NUT password and invokes nut-to-mqtt.
  wrapped = pkgs.writeShellApplication {
    name = "nut-to-mqtt-wrapped";
    runtimeInputs = [ pkgs.coreutils ];
    text = ''
      PASSWORD="$(cat "${config.sops.secrets.nut-password.path}")"
      exec "${cfg.binPath}" \
        -mqtt_clientid="" \
        -mqtt_endpoint="${cfg.mqttEndpoint}" \
        -nut_username="${cfg.nutUsername}" \
        -nut_password="$PASSWORD"
    '';
  };
in
{
  options.services.nutToMqtt = {
    enable = lib.mkEnableOption "NUT UPS to MQTT bridge";

    binPath = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/.local/bin/nut-to-mqtt";
      description = "Path to the nut-to-mqtt binary.";
    };

    mqttEndpoint = lib.mkOption {
      type = lib.types.str;
      default = "tcp://mqtt:1883/";
      description = "MQTT broker endpoint.";
    };

    nutUsername = lib.mkOption {
      type = lib.types.str;
      default = "upsmon";
      description = "NUT username to authenticate as.";
    };
  };

  config = lib.mkIf cfg.enable {
    sops.secrets.nut-password = {
      sopsFile = ../secrets/nut-password.sops.yaml;
      path = "${config.home.homeDirectory}/.config/sops-nix/secrets/nut-password";
    };

    home.packages = lib.optionals pkgs.stdenv.isLinux [ wrapped ];

    systemd.user.services.nuttomqtt = lib.mkIf pkgs.stdenv.isLinux {
      Unit = {
        Description = "nut to mqtt";
        After = "network.target";
      };
      Service = {
        ExecStart = "${wrapped}/bin/nut-to-mqtt-wrapped";
        Restart = "always";
        StartLimitInterval = 0;
        RestartSec = 60;
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
}
