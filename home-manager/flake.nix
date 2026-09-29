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
  };
  outputs = { nixpkgs, nixpkgs-old, home-manager, sops-nix, ... }:
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

      homeConfigurations = assert _consistencyCheck; builtins.listToAttrs (
        builtins.map (hostname:
          let
            system = systems.${hostname};
            isDarwin = lib.hasSuffix "-darwin" system;
            pkgs = nixpkgs.legacyPackages.${system};
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

      # The x86_64-linux hosts (aws, bee1, bee2) all evaluate against the
      # same nixpkgs, so their `home.packages` overlap heavily and can share
      # a cache. This buildEnv unions those packages -- derived from each
      # host's own evaluated config, so it never drifts out of sync with
      # common/shared.nix or machines/*.nix -- letting CI build it once as a
      # prerequisite, then build/push all three hosts concurrently instead of
      # chaining them just to get cache reuse.
      x86_64LinuxHosts = builtins.filter (h: systems.${h} == "x86_64-linux") (builtins.attrNames systems);
      x86_64LinuxSharedPackages = nixpkgs.legacyPackages.x86_64-linux.buildEnv {
        name = "ci-shared-x86_64-linux";
        paths = lib.unique (lib.concatMap
          (h: homeConfigurations."${username}@${h}".config.home.packages)
          x86_64LinuxHosts);
        ignoreCollisions = true;
      };
    in {
      inherit homeConfigurations;

      packages.x86_64-linux.ci-shared = x86_64LinuxSharedPackages;
    };
}
