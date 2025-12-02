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
      username = "dustin";
      
      # Define your systems with their architectures
      systems = {
        dsmac = "aarch64-darwin";
        dsstudio = "aarch64-darwin";
        aws = "x86_64-linux";
        bee1 = "x86_64-linux";
        bee2 = "x86_64-linux";
        pied = "aarch64-linux";
        thinky = "x86_64-linux";
      };
      
      # Generate all the homeConfigurations
      homeConfigurations = builtins.listToAttrs (
        builtins.map (hostname: 
          let 
            system = systems.${hostname};
            pkgs = nixpkgs.legacyPackages.${system};
            pkgs-old = nixpkgs-old.legacyPackages.${system};
          in {
            name = "${username}@${hostname}";
            value = home-manager.lib.homeManagerConfiguration {
              inherit pkgs;
              
              extraSpecialArgs = { inherit hostname pkgs-old; };
              
              modules = [
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
