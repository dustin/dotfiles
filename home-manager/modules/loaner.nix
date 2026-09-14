{ config, lib, pkgs, ... }:

let
  cfg = config.services.loaner;
in
{
  options.services.loaner = {
    enable = lib.mkEnableOption "loaner cabal run service";

    sourceDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/prog/loaner";
      description = "Directory containing the loaner cabal project.";
    };
  };

  config = lib.mkIf cfg.enable {
    sops.secrets.loaner-env = {
      sopsFile = ../secrets/loaner.sops.yaml;
      path = "${config.home.homeDirectory}/.config/sops-nix/secrets/loaner-env";
    };

    systemd.user.services.loaner = lib.mkIf pkgs.stdenv.isLinux {
      Unit = {
        Description = "loaner haskell service";
        After = [ "network.target" ];
      };
      Service = {
        Type = "simple";
        WorkingDirectory = cfg.sourceDir;
        EnvironmentFile = config.sops.secrets.loaner-env.path;
        ExecStart = "${pkgs.writeShellApplication {
          name = "loaner-run";
          runtimeInputs = [
            pkgs.coreutils
            pkgs.cacert
            pkgs.cabal-install
            pkgs.haskell.compiler.ghc96
            pkgs.pkg-config
            pkgs.postgresql
          ];
          text = ''
            export HOME="${config.home.homeDirectory}"
            export SSL_CERT_FILE="${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
            export NIX_SSL_CERT_FILE="${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
            export C_INCLUDE_PATH="${lib.makeSearchPathOutput "dev" "include" [
              pkgs.zlib
              pkgs.openssl
              pkgs.postgresql
            ]}''${C_INCLUDE_PATH:+:$C_INCLUDE_PATH}"
            export LIBRARY_PATH="${lib.makeSearchPath "lib" [
              pkgs.zlib
              pkgs.openssl
              pkgs.postgresql
            ]}''${LIBRARY_PATH:+:$LIBRARY_PATH}"
            export LD_LIBRARY_PATH="${lib.makeSearchPath "lib" [
              pkgs.zlib
              pkgs.openssl
              pkgs.postgresql
            ]}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
            export PKG_CONFIG_PATH="${lib.makeSearchPath "lib/pkgconfig" [
              pkgs.postgresql.dev
              pkgs.openssl.dev
              pkgs.zlib.dev
            ]}''${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
            export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$HOME/.local/bin:$HOME/.nix-profile/bin:$HOME/.local/state/nix/profile/bin:/etc/profiles/per-user/${config.home.username}/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:$PATH"
            cd "${cfg.sourceDir}"
            exec cabal run
          '';
        }}/bin/loaner-run";
        Restart = "always";
        StartLimitInterval = 0;
        RestartSec = 10;
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
}
