{
  description = "Pandoc development";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    nixpkgs-140774-workaround.url = "github:srid/nixpkgs-140774-workaround";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.main = {
          imports = [
            inputs.nixpkgs-140774-workaround.haskellFlakeProjectModules.default
          ];
          overrides = self: super: { };
          devShell = {
            tools = hp: {
              inherit (hp)
                hlint
                ormolu
                weeder
                stylish-haskell
                ghcid;
            };
            hlsCheck.enable = true;
          };
          packages = {
            pandoc.root = ./.;
            pandoc-cli.root = ./pandoc-cli;
            pandoc-server.root = ./pandoc-server;
            pandoc-lua-engine.root = ./pandoc-lua-engine;
          };
        };

        # Default package.
        packages.default = self'.packages.pandoc;

        devShells.default = self'.devShells.main;
      };
    };
}
