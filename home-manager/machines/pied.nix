{ config, pkgs, pkgs-old, ... }:

let
  s3bak = pkgs.callPackage ../pkgs/s3bak.nix { };
in
{
  my.secrets = {
    enable = true;
  };

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
          Requires = [ "mnt-books.mount" ];
        };
        Service = {
          ExecStartPre = ''-${pkgs.rsync}/bin/rsync -vaS --delete /mnt/books/calibre/ ${config.home.homeDirectory}/stuff/calibre/'';
          ExecStart = ''${pkgs.calibre}/bin/calibre-server ${config.home.homeDirectory}/stuff/calibre'';
          Restart = "always";
          StartLimitInterval = 0;
          RestartSec = 60;
          TimeoutStartSec = 600;
        };
      };
      s3bak = {
        Unit = {
          Description = "periodic s3 backup";
          After = "network.target";
        };
        Service = {
          Environment = [
            "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
            "NIX_SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
          ];
          ExecStart = pkgs.lib.getExe s3bak;
          Type = "oneshot";
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
    };
  };

}
