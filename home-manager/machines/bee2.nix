{ systemd, config, pkgs, ... }:

{
  imports = [
    ../common/shared.nix
    ../common/linux.nix
  ];

  home.packages = with pkgs; [
    static-web-server
    haskellPackages.net-mqtt # my mqtt-watch command
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
    };
  };

}
