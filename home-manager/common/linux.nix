{ systemd, config, pkgs, ... }:

{
    systemd.user = {
      services = {
        pueue = {
          Install = { Wantedby = ["default.target"]; };

          Unit = {
            Description = "pueue (batch/at thing)";
            After = "network.target";
          };

          Service = {
            ExecStart = "${pkgs.pueue}/bin/pueued";
            Restart = "always";
            StartLimitInterval = 0;
            RestartSec = 60;
          };
        };

      nixchanup = {
        Install = { WantedBy = ["default.target"]; };
        Unit = {
          Description = "Update nixpkg channels";
          After = "network.target";
        };
        Service = {
          Type = "oneshot";
          ExecStart = "${pkgs.nix}/bin/nix-channel --update";
        };
      };

      home-manager-cleanup = {
        Install = { WantedBy = ["default.target"]; };
        Unit = {
          Description = "Remove old home-manager generations";
          After = "network.target";
        };
        Service = {
          Type = "oneshot";
          ExecStart = "${pkgs.home-manager}/bin/home-manager expire-generations '-30 days'";
        };
      };
    };

    timers = {
      nixchanup = {
        Install = { WantedBy = [ "timers.target" ]; };
        Timer = {
          OnCalendar = "daily";
          RandomizedDelaySec = "900";
          Unit = "nixchanup.service";
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
    };
  };

}
