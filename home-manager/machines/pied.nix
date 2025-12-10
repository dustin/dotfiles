{ systemd, config, pkgs, pkgs-old, ... }:

{
  imports = [
    ../common/shared.nix
    ../common/linux.nix
  ];

  home.packages = with pkgs; [
    calibre
    pkgs-old.haskellPackages.net-mqtt # my mqtt-watch command
  ];

  systemd.user = {
    services = {
      calibre-server = {
        Install = { WantedBy = ["default.target"]; };
        Unit = {
          Description = "calibre-server";
          After = "network.target";
        };
        Service = {
          ExecStartPre = ''-${pkgs.rsync}/bin/rsync -vaS --delete /mnt/books/calibre/ /home/dustin/stuff/calibre/'';
          ExecStart = ''${pkgs.calibre}/bin/calibre-server /home/dustin/stuff/calibre'';
          Restart = "always";
          StartLimitInterval = 0;
          RestartSec = 60;
            TimeoutStartSec = 300;
        };
      };
      s3bak = {
        Install = { WantedBy = ["default.target"]; };
        Unit = {
          Description = "periodic s3 backup";
          After = "network.target";
        };
        Service = {
          Environment="RCLONE=${pkgs.rclone}/bin/rclone";
          ExecStart = "/home/dustin/.local/bin/s3bak";
          Type = "oneshot";
        };
      };
      teslauth = {
        Install = { WantedBy = ["default.target"]; };

        Unit = {
          Description = "tesladb";
          After = "network.target";
        };

        Service = {
          Type = "oneshot";
          ExecStart = ''${pkgs.docker}/bin/docker run --rm \
            -e TZ=Pacific/Honolulu \
            --user 1000:100 \
            -v /home/dustin/stuff/tesladb:/data \
            --entrypoint teslauth \
            dustin/tesladb -r --dbpath=tesla.db'';
        };
      };
    };
    timers = {
      s3bak = {
         Install = { WantedBy = [ "timers.target" ]; };
         Timer = {
           OnCalendar = "daily";
           RandomizedDelaySec = "900";
           Unit = "s3bak.service";
         };
      };
      home-manager-cleanup = {
        Install = { WantedBy = [ "timers.target" ]; };
        Timer = {
          OnCalendar = "daily";
          RandomizedDelaySec = "900";
          Unit = "home-manager-cleanup.service";
        };
      };
      teslauth = {
        Install = { WantedBy = [ "timers.target" ]; };
        Timer = {
          OnBootSec = "300";
          OnUnitActiveSec = "7200"; # should expire after 28800
          RandomizedDelaySec = "900";
          Unit = "teslauth.service";
        };
      };
    };
  };

}
