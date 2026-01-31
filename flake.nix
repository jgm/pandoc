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
        # Or, override some dependencies as follows:
        # haskellPackages = (pkgs.haskellPackages.override {
        #   all-cabal-hashes = pkgs.fetchurl {
        #    # The hash in the URL is just a git commit hash
        #    url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/2e3f153549871ada6ecbf36719339a5da051bc76.tar.gz";
        #    sha256 = "ymHZb6wCZlrtKb+T+iOL17jyN8IzA7s52uiamUIkWNI=";
        #   };
        # }).extend(self: super: {
        #   citeproc = pkgs.haskell.lib.dontCheck (self.callHackage "citeproc" "0.9.0.1" {});
        #   texmath = self.callHackage "texmath" "0.12.10.2" {};
        # });

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        # PUT YOUR PACKAGE NAME HERE:
        packageName = "pandoc";

        wasmToolchain = ghc-wasm-meta.packages.${system}.default;
        # Alternatively, if you want a specific "bundle":
        # wasmToolchain = ghc-wasm-meta.packages.${system}.all_9_14;
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
          };

        packages.default = self.packages.${system}.${packageName};
        defaultPackage = self.packages.${system}.default;

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
          inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
        };

        devShell = self.devShells.${system}.default;
      });
}
