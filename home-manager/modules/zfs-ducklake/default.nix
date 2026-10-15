# zfs-ducklake-sync: puts `zfs-sync-to-ducklake` on the PATH. It merges
# the per-host ZFS manifest dbs (nixos-configs modules/zfs-manifest,
# shared from lilnasx) into the DuckLake. Run by hand, e.g.
#   zfs-sync-to-ducklake --init-sql ~/stuff/duck/init.sql
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
  options.programs.zfsDucklakeSync.enable =
    lib.mkEnableOption "the zfs-sync-to-ducklake command";

  config = lib.mkIf cfg.enable {
    home.packages = [ sync ];
  };
}
