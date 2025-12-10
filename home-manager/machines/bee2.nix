{ systemd, config, pkgs, pkgs-old, ... }:

{
  imports = [
    ../common/shared.nix
    ../common/linux.nix
  ];

  home.packages = with pkgs; [
    static-web-server
    pkgs-old.haskellPackages.net-mqtt # my mqtt-watch command
  ];

  systemd.user = {
    services = {
      static-web-server = {
        Install = { WantedBy = ["default.target"]; };
        Unit = {
          Description = "static web server";
          After = "network.target";
        };
        Service = {
          ExecStart = ''${pkgs.static-web-server}/bin/static-web-server --ignore-hidden-files -w /home/dustin/stuff/sws.toml'';
          Restart = "always";
          StartLimitInterval = 0;
          RestartSec = 60;
          TimeoutStartSec = 300;
        };
      };

      nuttomqtt = {
        Install = { WantedBy = ["default.target"]; };

        Unit = {
          Description = "nut to mqtt";
          After = "network.target";
        };

        Service = {
          ExecStart = ''/home/dustin/.local/bin/nut-to-mqtt -mqtt_clientid="" -mqtt_endpoint=tcp://mqtt:1883/ -nut_username=upsmon -nut_password=somepassword'';
          Restart = ''always'';
          StartLimitInterval = 0;
          RestartSec = 60;
        };
      };

    };
  };

}
