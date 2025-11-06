{
  description = "flake for ayys";

  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    ayys-st = {
      url = "github:ayys/st/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    musu = {
      url = "github:ayys/musu/2ebd98c";
      inputs.nixpkgs.follows = "nixpkgs";
    };    
    wasmenv = {
      url = "github:ayys/wasmenv/0.2.6";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    lla = {
      url = "github:triyanox/lla";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust-overlay = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, home-manager, ... }: {
    nixosConfigurations = {
      ayys = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./configuration.nix
          home-manager.nixosModules.default
          {
            home-manager.backupFileExtension = "backup";
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.ayys = import ./home.nix;
          }
        ];
      };
    };
  };
}
