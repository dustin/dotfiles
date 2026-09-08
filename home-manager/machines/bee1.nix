{ systemd, config, pkgs-old, pkgs, ... }:

{
  imports = [
    ../common/zfstos3.nix
  ];

  my.secrets = {
    enable = true;
    nut-password.enable = true;
  };

  zfstos3 = {
    datasets = [
      "zpool/var/lib/postgresql"
      "zpool/data/immich/postgres"
      "zpool/data/immich"
      "zpool/var/lib/influxdb"
    ];
    ageRecipient = "age17l4lq89zpdyzlg37ktjtauyq60wnwuw0nj74rax67k9caldck9js63lleu";
  };

  home.packages = with pkgs; [
    pkgs-old.haskellPackages.net-mqtt # my mqtt-watch command
    pgcli
	# rtl-sdr-librtlsdr
	rtl-sdr
	libusb1
  ];

  systemd.user = {
    services = {

      rtl433 = {
        Install = { WantedBy = ["default.target"]; };

        Unit = {
          Description = "rtl sdr";
          After = "network.target";
        };

        Service = {
          ExecStart = ''${pkgs.rtl_433}/bin/rtl_433 -F mqtt://localhost:1883,user=rtl433,retain=0,devices=rtl_433[/id]'';
          Restart = ''always'';
          StartLimitInterval = 0;
          RestartSec = 60;
        };
      };

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

      gitmirror = {
        Install = { WantedBy = ["default.target"]; };

        Unit = {
          Description = "gitmirror";
          After = "network.target";
        };

        Service = {
          ExecStart = ''/home/dustin/.local/bin/gitmirror -dir /mnt/dustin/stuff/gitmirror -proto https -git /home/dustin/.nix-profile/bin/git'';
          Restart = ''always'';
          StartLimitInterval = 0;
          RestartSec = 60;
        };
      };

      papertrails = {
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
