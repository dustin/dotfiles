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
                ./modules/metube-transcode.nix
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

      # Every package actually installed across all hosts. Used below to
      # figure out which of them are plain nixpkgs packages worth watching
      # for updates.
      allHomePackages = lib.concatMap
        (name: homeConfigurations.${name}.config.home.packages)
        (builtins.attrNames homeConfigurations);

      # `pname` -> current version for every package in `home.packages`,
      # across all hosts, that is a plain top-level nixpkgs attribute (e.g.
      # `pkgs.duckdb`). This is derived automatically -- no hand-maintained
      # package list -- by checking whether
      # `nixpkgs.legacyPackages.<system>.<pname>` resolves to that exact same
      # derivation. That excludes local wrappers built via `pkgs.callPackage
      # ../pkgs/*.nix` (e.g. headroom, centauri,
      # bambu-weight-fetcher; they have no matching top-level attribute) and
      # packages reached through a nested attribute path or a different
      # nixpkgs pin (e.g. `pkgs-old.haskellPackages.net-mqtt`).
      #
      # Evaluate this same output again with
      # `--override-input nixpkgs github:nixos/nixpkgs/nixpkgs-unstable` to
      # see what's currently available upstream, using the exact same
      # filtering logic -- no separate script to keep in sync.
      trackedVersions =
        let
          systemsUsed = lib.unique (map (pkg: pkg.system) allHomePackages);
          versionsForSystem = system:
            let
              pkgsForSystem = nixpkgs.legacyPackages.${system};
              candidates = builtins.filter (pkg: pkg.system == system) allHomePackages;
              trackable = builtins.filter (pkg:
                let
                  pname = pkg.pname or null;
                  # Some pnames are retired aliases that just `throw` when
                  # accessed (e.g. `dust` -> `du-dust`), so this lookup has to
                  # be wrapped in tryEval rather than a plain `or null`.
                  topLevelEval =
                    if pname == null then { success = false; }
                    else builtins.tryEval (pkgsForSystem.${pname} or null);
                in
                  topLevelEval.success
                  && topLevelEval.value != null
                  && topLevelEval.value.outPath == pkg.outPath
              ) candidates;
            in
              lib.listToAttrs (map (pkg: {
                name = pkg.pname;
                value = pkg.version or "unknown";
              }) trackable);
        in
          lib.genAttrs systemsUsed versionsForSystem;
    in {
      inherit homeConfigurations trackedVersions;

      packages.x86_64-linux.ci-shared = x86_64LinuxSharedPackages;
    };
}
