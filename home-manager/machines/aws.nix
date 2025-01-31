{ systemd, config, pkgs, ... }:

let mypkgs = {
   mqtt-bridge = "/home/dustin/.nix-profile";
   influxer = "/home/dustin/.nix-profile";
   outfluxer = "/home/dustin/.nix-profile";
   babysitter = "/home/dustin/.nix-profile";
   papertrails = "/home/dustin/.nix-profile";
  };
in

{
  imports = [
    ../common/shared.nix
  ];

  home.packages = with pkgs; [
    awscli2
  ];

  systemd.user = {
    services = {
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

  programs.zsh.initExtra = ''
  '';
}
