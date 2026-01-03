{
  description = "pandoc";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
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
        #   commonmark-pandoc = self.callHackage "commonmark-pandoc" "0.2.3" {};
        #   typst-symbols = self.callHackage "typst-symbols" "0.1.8.1" {};
        #   typst = self.callHackage "typst" "0.8" {};
        #   texmath = self.callHackage "texmath" "0.12.10.2" {};
        #   toml-parser = self.callHackage "toml-parser" "2.0.1.2" {};
        # });

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        # DON'T FORGET TO PUT YOUR PACKAGE NAME HERE, REMOVING `throw`
        packageName = "pandoc";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
          };

        packages.default = self.packages.${system}.${packageName};
        defaultPackage = self.packages.${system}.default;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            haskellPackages.hlint
            haskellPackages.cabal-install
            haskellPackages.cabal-plan
            haskellPackages.weeder
            haskellPackages.hpc
            haskellPackages.ghcid
            haskellPackages.stylish-haskell
            haskellPackages.eventlog2html
            haskellPackages.profiterole
            haskellPackages.profiteur
            zlib.dev
            git
            bashInteractive
            epubcheck # for validate-epub
            nodejs # for validate-epub
            ripgrep
            libxml2 # for xmllint
            jq
          ];
          inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
        };
        devShell = self.devShells.${system}.default;
      });
}
