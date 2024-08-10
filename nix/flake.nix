# /etc/nixos/flake.nix
{
  description = "flake for ayys";

  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
  };

  outputs = { self, nixpkgs }: {
    nixosConfigurations = {
      ayys = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ../configuration.nix
        ];
      };
    };
  };
}
