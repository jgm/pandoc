# This skips cabal2nix and doesn't bring in any Haskell libraries from nix.
{
  description = "pandoc";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
  };

  outputs = { self, nixpkgs, flake-utils, ghc-wasm-meta }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
        wasmToolchain = ghc-wasm-meta.packages.${system}.default;
        # Alternatively, if you want a specific "bundle":
        # wasmToolchain = ghc-wasm-meta.packages.${system}.all_9_14;
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            wasmToolchain
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.cabal-install
            haskellPackages.cabal-plan
            haskellPackages.hpc
            haskellPackages.ghcid
            haskellPackages.stylish-haskell
            haskellPackages.eventlog2html
            haskellPackages.profiterole
            haskellPackages.profiteur
            zlib.dev
            git
            gnumake
            bashInteractive
            ripgrep
            unzip
            jq
            libxml2 # for xmllint
            epubcheck # for validate-epub
            nodejs # for validate-epub
          ];
        };
      });
}
