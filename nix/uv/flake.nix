{
  description = "uv: Extremely fast Python package installer and resolver, written in Rust";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    fenix.url = "github:nix-community/fenix";
  };

  outputs = { self, nixpkgs, fenix, ... }: let
    system = "x86_64-linux";  # Change as needed
    pkgs = import nixpkgs { inherit system; };
  in {
    defaultPackage.${system} = self.packages.${system}.uv;

    name = "uv";
    version = "0.6.9";
    
    src = pkgs.fetchFromGitHub {
      owner = "astral-sh";
      repo = "uv";
      rev = self.version;
      hash = "sha256-i/NnJpLAf7pDxzIuV+30yhbPJT9+2iZcr8x4qDfxUXc=";
    };

    rust-toolchain = fenix.packages.x86_64-linux.fromToolchainFile {
      file = "${self.src}/rust-toolchain.toml";
      sha256 = "sha256-Hn2uaQzRLidAWpfmRwSRdImifGUCAb9HeAqTYFXWeQk=";
    };
    
    packages.${system}.uv = pkgs.python3Packages.buildPythonPackage rec {
      src = self.src;
      version = self.version;
      pname = self.name;
      nativeBuildInputs = [
        pkgs.cmake
        pkgs.installShellFiles
        pkgs.pkg-config
        self.rust-toolchain
        pkgs.rustPlatform.cargoSetupHook
        pkgs.rustPlatform.maturinBuildHook
        pkgs.tree
      ];

      dontUseCmakeConfigure = true;

      cargoDeps = pkgs.rustPlatform.importCargoLock {
        lockFile = "${src}/Cargo.lock";
        outputHashes = {
          "async_zip-0.0.17" = "sha256-VfQg2ZY5F2cFoYQZrtf2DHj0lWgivZtFaFJKZ4oyYdo=";
          "pubgrub-0.3.0-alpha.1" = "sha256-FF10Ia2fvBIP/toxnjh/bqjHazFDChMd2qQzASGZLiM=";
          "tl-0.7.8" = "sha256-F06zVeSZA4adT6AzLzz1i9uxpI1b8P1h+05fFfjm3GQ=";
        };
      };

      cargoBuildFlags = [
        "--package"
        "uv"
      ];

      pythonImportsCheck = [ "uv" ];

      nativeCheckInputs = [
        pkgs.versionCheckHook
      ];

      versionCheckProgramArg = [ "--version" ];

      postInstall = ''
        export HOME=$TMPDIR
        installShellCompletion --cmd uv \
          --bash <($out/bin/uv --generate-shell-completion bash) \
          --fish <($out/bin/uv --generate-shell-completion fish) \
          --zsh <($out/bin/uv --generate-shell-completion zsh)
      '';

      meta = with pkgs.lib; {
        description = "Extremely fast Python package installer and resolver, written in Rust";
        homepage = "https://github.com/astral-sh/uv";
        changelog = "https://github.com/astral-sh/uv/blob/${src.rev}/CHANGELOG.md";
        license = licenses.asl20;
        maintainers = [ maintainers.GaetanLepage ];
        mainProgram = "uv";
      };
    };
  };

}
