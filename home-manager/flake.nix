{
  description = "Dustin's Home Manager configuration";
  inputs = {
    nixpkgs-old.url = "github:NixOS/nixpkgs/c53baa6685261e5253a1c355a1b322f82674a824";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { nixpkgs, nixpkgs-old, home-manager, ... }:
    let
      lib = nixpkgs.lib;
      username = "dustin";

      systems = {
        dsmac = "aarch64-darwin";
        dsstudio = "aarch64-darwin";
        aws = "x86_64-linux";
        bee1 = "x86_64-linux";
        bee2 = "x86_64-linux";
        pied = "aarch64-linux";
        thinky = "x86_64-linux";
      };

      machineFiles = builtins.attrNames (
        lib.filterAttrs (name: type: type == "regular" && name != "default.nix")
          (builtins.readDir ./machines)
      );
      machineHosts = map (f: lib.removeSuffix ".nix" f) machineFiles;
      registeredHosts = builtins.attrNames systems;
      missingFromSystems = lib.subtractLists registeredHosts machineHosts;
      missingFile = lib.subtractLists machineHosts registeredHosts;
      _consistencyCheck =
        if missingFromSystems != [ ] then
          throw "machines/*.nix exist with no `systems` entry in flake.nix: ${toString missingFromSystems}"
        else if missingFile != [ ] then
          throw "`systems` entries in flake.nix have no matching machines/*.nix file: ${toString missingFile}"
        else true;

      # Bump duckdb past what's in nixpkgs
      duckdbOverlay = final: prev: {
        duckdb = prev.duckdb.overrideAttrs (old: {
          version = "1.5.5";

          src = final.fetchFromGitHub {
            owner = "duckdb";
            repo = "duckdb";
            tag = "v1.5.5";
            hash = "sha256-vFXrMcWF5KDYYRjWZb6iJdhGnCAb6SMlSgzlcr+FQ8Y=";
          };

          cmakeFlags =
            (final.lib.filter
              (f: !(final.lib.hasInfix "OVERRIDE_GIT_DESCRIBE" f))
              old.cmakeFlags)
            ++ [
              (final.lib.cmakeFeature "OVERRIDE_GIT_DESCRIBE"
                "v1.5.5-0-gd8cdaa33fda8df955cc76ef58a280f68f4cd43fa")
            ];

          # doInstallCheck = false; # uncomment if the test suite chokes
        });
      };

      homeConfigurations = assert _consistencyCheck; builtins.listToAttrs (
        builtins.map (hostname:
          let
            system = systems.${hostname};
            isDarwin = lib.hasSuffix "-darwin" system;
            pkgs = (nixpkgs.legacyPackages.${system}).extend duckdbOverlay;
            pkgs-old = nixpkgs-old.legacyPackages.${system};
          in {
            name = "${username}@${hostname}";
            value = home-manager.lib.homeManagerConfiguration {
              inherit pkgs;

              extraSpecialArgs = { inherit hostname pkgs-old; };

              modules = [
                ./common/shared.nix
                (if isDarwin then ./common/darwin.nix else ./common/linux.nix)
                ./machines/${hostname}.nix
                {
                  nixpkgs.config.allowUnfree = true;
                  nixpkgs.config.packageOverrides = pkgs: {
                    unstable = nixpkgs.legacyPackages.${system};
                  };
                }
              ];
            };
          }
        ) (builtins.attrNames systems)
      );
    in {
      inherit homeConfigurations;
    };
}
