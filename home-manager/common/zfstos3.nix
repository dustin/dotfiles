# zfstos3.nix

{ config, lib, pkgs, hostname, ... }:

with lib;

let
  cfg = config.zfstos3;

  zfsSendToS3 = pkgs.writeShellApplication {
    name = "zfs-send-to-s3";
    runtimeInputs = [ pkgs.zfs ];
    text = builtins.readFile ./zfs-send-to-s3.sh;
  };

  execStart = "${zfsSendToS3}/bin/zfs-send-to-s3 ${builtins.concatStringsSep " " cfg.datasets}";
in
{
  options.zfstos3 = {
    datasets = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "ZFS datasets to snapshot and send to S3 on this machine.";
      example = [ "zpool/var/lib/postgresql" "zpool/data/immich" ];
    };

    ageRecipient = mkOption {
      type = types.str;
      description = "age public key (age1...) to encrypt backup streams to.";
    };

    prefix = mkOption {
      type = types.str;
      default = hostname;
      description = "S3 object key prefix for this machine's backups. Defaults to the hostname.";
      example = "bee1";
    };

    bucket = mkOption {
      type = types.str;
      default = "backup.west.spy.net";
      description = "S3 bucket to send backups to. Shared default; override per-machine if needed.";
    };

    dailyTime = mkOption {
      type = types.str;
      default = "*-*-* 03:00:00";
      description = "systemd OnCalendar expression for the daily incremental run.";
    };

    monthlyFullTime = mkOption {
      type = types.str;
      default = "*-*-01 04:30:00";
      description = "systemd OnCalendar expression for the monthly full re-base run.";
    };
  };

  config = mkIf (cfg.datasets != [ ]) {
    home.packages = [ zfsSendToS3 ];

    systemd.user.services = {
      zfstos3-daily = {
        Unit = {
          Description = "Incremental ZFS backup to S3";
          After = [ "network-online.target" ];
          Wants = [ "network-online.target" ];
          Conflicts = [ "zfstos3-monthly-full.service" ];
        };
        Service = {
          Type = "oneshot";
          Environment = [
            "AGE_RECIPIENT=${cfg.ageRecipient}"
            "PREFIX=${cfg.prefix}"
            "BUCKET=${cfg.bucket}"
          ];
          ExecStart = execStart;
        };
      };

      zfstos3-monthly-full = {
        Unit = {
          Description = "Full re-base ZFS backup to S3";
          After = [ "network-online.target" ];
          Wants = [ "network-online.target" ];
          Conflicts = [ "zfstos3-daily.service" ];
        };
        Service = {
          Type = "oneshot";
          Environment = [
            "AGE_RECIPIENT=${cfg.ageRecipient}"
            "PREFIX=${cfg.prefix}"
            "BUCKET=${cfg.bucket}"
            "FORCE_FULL=1"
          ];
          ExecStart = execStart;
        };
      };
    };

    systemd.user.timers = {
      zfstos3-daily = {
        Install.WantedBy = [ "timers.target" ];
        Timer = {
          OnCalendar = cfg.dailyTime;
          Persistent = true;
          RandomizedDelaySec = "10m";
        };
      };

      zfstos3-monthly-full = {
        Install.WantedBy = [ "timers.target" ];
        Timer = {
          OnCalendar = cfg.monthlyFullTime;
          Persistent = true;
          RandomizedDelaySec = "10m";
        };
      };
    };
  };
}
