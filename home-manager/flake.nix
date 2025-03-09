{
  description = "Dustin's Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, ... }: 
    let
      system = "aarch64-darwin";  # For Apple Silicon Macs
      pkgs = nixpkgs.legacyPackages.${system};
      lib = nixpkgs.lib;  # Import lib from nixpkgs
    in {
      homeConfigurations = {
        "dustin@dsmac" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          
          modules = [
            ./machines/dsmac.nix
            # Add machine-specific configuration
            {
              # Define a custom option to identify the machine
              _module.args.currentSystem = "dsmac";
              
              # Machine-specific configuration here
              programs.zsh.initExtra = lib.mkAfter ''
                # dsmac-specific zsh configuration
                # Add any machine-specific configuration here
              '';
            }
          ];
        };
      };
    };
}
