# zfs-ducklake-sync: puts `zfs-sync-to-ducklake` on the PATH and runs it
# daily from launchd. It merges the per-host ZFS manifest dbs
# (nixos-configs modules/zfs-manifest, shared from lilnasx) into the
# DuckLake. The agent runs in workingDirectory, where the script finds
# init.sql by default. Schema setup and --rebuild stay manual:
#   cd ~/stuff/duck && zfs-sync-to-ducklake --rebuild
# The schema file is packaged next to the script, where it looks for it.
{ config, lib, pkgs, ... }:

let
  cfg = config.programs.zfsDucklakeSync;
  scriptDir = pkgs.runCommand "zfs-ducklake-sync-scripts" { } ''
    mkdir $out
    cp ${./zfs-sync-to-ducklake.sh} $out/zfs-sync-to-ducklake.sh
    cp ${./zfs-ducklake-schema.sql} $out/zfs-ducklake-schema.sql
  '';
  sync = pkgs.writeShellScriptBin "zfs-sync-to-ducklake" ''
    export PATH=${lib.makeBinPath [ pkgs.duckdb pkgs.coreutils pkgs.gnused pkgs.gnugrep ]}:$PATH
    exec ${pkgs.bash}/bin/bash ${scriptDir}/zfs-sync-to-ducklake.sh "$@"
  '';
in
{
  options.programs.zfsDucklakeSync = {
    enable = lib.mkEnableOption "the zfs-sync-to-ducklake command and its daily launchd agent";

    workingDirectory = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/stuff/duck";
      description = "Where the agent runs; must hold init.sql.";
    };

    hour = lib.mkOption {
      type = lib.types.int;
      default = 5;
      description = "Hour (24-hour) of the daily sync, after the hosts' 02:00 enqueue and worker.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ sync ];

    launchd.agents.zfs-ducklake-sync = {
      enable = true;
      config = {
        Label = "net.spy.zfs-ducklake-sync";
        ProgramArguments = [ "${sync}/bin/zfs-sync-to-ducklake" ];
        RunAtLoad = false;
        StartCalendarInterval = [ { Hour = cfg.hour; Minute = 0; } ];
        KeepAlive = false;
        StandardOutPath  = "${config.xdg.stateHome}/zfs-ducklake-sync/stdout.log";
        StandardErrorPath = "${config.xdg.stateHome}/zfs-ducklake-sync/stderr.log";
        EnvironmentVariables = {
          HOME = config.home.homeDirectory;
          LANG = "en_US.UTF-8";
          SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
          NIX_SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        };
        WorkingDirectory = cfg.workingDirectory;
      };
    };
  };
}
