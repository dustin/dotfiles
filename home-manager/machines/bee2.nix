{
  config,
  pkgs,
  pkgs-old,
  ...
}:

{
  my.secrets = {
    enable = true;
    nut-password.enable = true;
  };

  home.packages = with pkgs; [
    static-web-server
    pkgs-old.haskellPackages.net-mqtt # my mqtt-watch command
  ];

  systemd.user = {
    services = {

      nuttomqtt = {
        Install = { WantedBy = ["default.target"]; };

        Unit = {
          Description = "nut to mqtt";
          After = "network.target";
        };

        Service = {
          ExecStart = "${config.home.homeDirectory}/.nix-profile/bin/nut-to-mqtt-wrapped";
          Restart = ''always'';
          StartLimitInterval = 0;
          RestartSec = 60;
        };
      };

    };
  };

}
