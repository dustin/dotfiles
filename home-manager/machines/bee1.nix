{ systemd, config, pkgs, ... }:

let mypkgs = {
   # docker = "/nix/store/c479jrs5i9nx16hrgysncdcc3jhcn4qd-docker-20.10.23";
   docker = "${pkgs.docker}";
  };
in

{
  imports = [
    ../common/shared.nix
    ../common/linux.nix
  ];

  home.packages = with pkgs; [
    haskellPackages.net-mqtt # my mqtt-watch command
  ];

  systemd.user = {
    services = {
      babysitter = {
        Install = { WantedBy = ["default.target"]; };

        Unit = {
          Description = "babysitter";
          After = "network.target";
        };

        Service = {
          ExecStart = ''/home/dustin/.local/bin/babysitter --conf /home/dustin/babysitter.conf --delay 300'';
          Restart = ''always'';
          StartLimitInterval = 0;
          RestartSec = 60;
        };
      };

      rtl433 = {
        Install = { WantedBy = ["default.target"]; };

        Unit = {
          Description = "rtl sdr";
          After = "network.target";
        };

        Service = {
          # ExecStart = ''${pkgs.rtl_433}/bin/rtl_433 -F mqtt://localhost:1883,user=rtl433,retain=0,devices=rtl_433[/id]'';
          ExecStart = ''/home/dustin/.local/bin/rtl_433 -F mqtt://localhost:1883,user=rtl433,retain=0,events=rtl_433[/model][/id]'';
          Restart = ''always'';
          StartLimitInterval = 0;
          RestartSec = 60;
        };
      };

      gitmirror = {
        Install = { WantedBy = ["default.target"]; };

        Unit = {
          Description = "gitmirror";
          After = "network.target";
        };

        Service = {
          ExecStart = ''/home/dustin/.local/bin/gitmirror -dir /mnt/dustin/stuff/gitmirror -proto https -git /home/dustin/.nix-profil
e/bin/git'';
          Restart = ''always'';
          StartLimitInterval = 0;
          RestartSec = 60;
        };
      };
      immich = {
        Install = { WantedBy = ["default.target"]; };

        Unit = {
          Description = "immich photo service";
          After = "network.target";
        };

        Service = {
          WorkingDirectory = "/home/dustin/stuff/immich-app";
          ExecStartPre = [
        ''-${mypkgs.docker}/bin/docker compose down''
            ''-${mypkgs.docker}/bin/docker compose pull''];
          ExecStart = ''${mypkgs.docker}/bin/docker compose up'';
          ExecStop = ''${mypkgs.docker}/bin/docker compose down'';
          Restart = ''always'';
          StartLimitInterval = 0;
          RestartSec = 60;
          TimeoutStartSec = 900;
        };
      };

      papertrails = {
        Install = { WantedBy = ["default.target"]; };
        Unit = {
          Description = "Aggregate and persist logs";
          After = "network.target";
        };
        Service = {
          Type = "oneshot";
          WorkingDirectory = "/home/dustin/prog/papertrails";
          Environment = "PATH=/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:${pkgs.p7zip}/bin";
          ExecStart = "/home/dustin/.local//bin/papertrails --bucket=logarchive.west.spy.net";
        };
      };

    };

    timers = {
      papertrails = {
        Install = { WantedBy = [ "timers.target" ]; };
        Timer = {
          OnCalendar = "*-*-01 03:04:05";
          RandomizedDelaySec = "900";
          Unit = "papertrails.service";
        };
      };
    };
  };

}
