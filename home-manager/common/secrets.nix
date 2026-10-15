{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.secrets;

  # Every rclone config built from secrets/rclone/ fragments.
  rcloneConfigs = cfg.rclone-config.extra
    // optionalAttrs (cfg.rclone-config.enable && cfg.rclone-config.remotes != [ ]) {
      default = {
        inherit (cfg.rclone-config) remotes;
        path = "${config.home.homeDirectory}/.config/rclone/rclone.conf";
      };
    };
in
{
  options.my.secrets = {
    enable = mkEnableOption "sops-nix secret management on this machine";

    aws-credentials = {
      enable = mkEnableOption "AWS credentials file";
    };

    rclone-config = {
      enable = mkEnableOption "rclone config file";

      remotes = mkOption {
        type = types.listOf types.str;
        default = [ ];
        example = [ "s3" ];
        description = ''
          rclone remotes this machine gets. Each one is a config fragment in
          secrets/rclone/<name>.conf.sops; the fragments are concatenated into
          ~/.config/rclone/rclone.conf. Empty means the legacy all-remotes
          secrets/rclone.conf.sops.
        '';
      };

      extra = mkOption {
        type = types.attrsOf (types.submodule ({ name, ... }: {
          options = {
            remotes = mkOption {
              type = types.listOf types.str;
              description = "Fragments from secrets/rclone/ joined into this config.";
            };
            path = mkOption {
              type = types.str;
              default = "${config.home.homeDirectory}/.config/rclone/${name}.conf";
              description = "Where the config is installed.";
            };
          };
        }));
        default = { };
        example = { dwp-bot.remotes = [ "walter" ]; };
        description = ''
          Separate rclone configs for services that should only see their own
          remotes, independent of rclone-config.enable. Point the service at
          one with RCLONE_CONFIG.
        '';
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # sops CLI for editing secrets.
      home.packages = [ pkgs.sops ];

      # Use the existing age key location from the agenix attempt.
      sops.age.keyFile = "${config.home.homeDirectory}/.config/age/keys.txt";

      # Common secrets: installed by default on any machine with secrets enabled.
      my.secrets.aws-credentials.enable = mkDefault true;
      my.secrets.rclone-config.enable = mkDefault true;
    }

    (mkIf cfg.aws-credentials.enable {
      sops.secrets.aws-credentials = {
        sopsFile = ../secrets/aws-credentials.sops.yaml;
        path = "${config.home.homeDirectory}/.aws/credentials";
      };

      home.file = {
        ".aws/.keep".text = "";
      };
    })

    (mkIf cfg.rclone-config.enable {
      home.file = {
        ".config/rclone/.keep".text = "";
      };
    })

    (mkIf (cfg.rclone-config.enable && cfg.rclone-config.remotes == [ ]) {
      sops.secrets.rclone-config = {
        sopsFile = ../secrets/rclone.conf.sops;
        format = "binary";
        path = "${config.home.homeDirectory}/.config/rclone/rclone.conf";
      };
    })

    {
      sops.secrets = listToAttrs (map (r: nameValuePair "rclone-${r}" {
        sopsFile = ../secrets/rclone + "/${r}.conf.sops";
        format = "binary";
      }) (unique (concatLists (mapAttrsToList (_: c: c.remotes) rcloneConfigs))));

      sops.templates = mapAttrs' (name: c: nameValuePair "rclone-${name}.conf" {
        content = concatMapStringsSep "\n" (r: config.sops.placeholder."rclone-${r}") c.remotes;
        inherit (c) path;
      }) rcloneConfigs;
    }
  ]);
}
