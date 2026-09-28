{
  description = "Dustin's Home Manager configuration";
  inputs = {
    nixpkgs-old.url = "github:NixOS/nixpkgs/c53baa6685261e5253a1c355a1b322f82674a824";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Bump duckdb past what's in nixpkgs. To upgrade: bump the tag in this
    # url, then run `nix flake update duckdb-src`.
    duckdb-src = {
      url = "github:duckdb/duckdb/v1.5.6";
      flake = false;
    };
  };
  outputs = { nixpkgs, nixpkgs-old, home-manager, sops-nix, duckdb-src, ... }:
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

      # Bump duckdb past what's in nixpkgs. The tag and rev come from
      # `duckdb-src` (see inputs above), which flake.lock pins to an exact
      # commit/hash, so there's nothing to hand-copy here. To upgrade: change
      # the tag in the `duckdb-src` input url, then run
      # `nix flake update duckdb-src`.
      duckdbTag = (builtins.fromJSON (builtins.readFile ./flake.lock))
        .nodes.duckdb-src.original.ref;
      duckdbVersion = lib.removePrefix "v" duckdbTag;

      duckdbOverlay = final: prev: {
        duckdb = prev.duckdb.overrideAttrs (old: {
          version = duckdbVersion;
          src = duckdb-src;

          # duckdb embeds `git describe` output into its build; since
          # `duckdb-src` is fetched as a plain tree (no .git dir) we have to
          # fake that output here.
          cmakeFlags =
            (final.lib.filter
              (f: !(final.lib.hasInfix "OVERRIDE_GIT_DESCRIBE" f))
              old.cmakeFlags)
            ++ [
              (final.lib.cmakeFeature "OVERRIDE_GIT_DESCRIBE"
                "${duckdbTag}-0-g${duckdb-src.rev}")
            ];

          # doInstallCheck = false; # uncomment if the test suite chokes
          # patches = [ ]; # uncomment if nixpkgs' patches stop applying to the newer tag
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
                sops-nix.homeManagerModules.sops
                ./modules/headroom.nix
                ./modules/9router.nix
                ./modules/nut-to-mqtt.nix
                ./modules/loaner.nix
                ./modules/bambu-weight-fetcher.nix
                ./modules/laya.nix
                ./common/shared.nix
                ./common/secrets.nix
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
