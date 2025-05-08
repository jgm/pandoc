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
        #    url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/c10b0447a6de06e7d226477787225fa1136abb95.tar.gz";
        #    sha256 = "9XkvkmfiRvkqGw9dpgTPXXdjaOQN1W9j1dOEDwUGnwM=";
        #   };
        # }).extend(self: super: {
        #   typst-symbols = self.callHackage "typst-symbols" "0.1.7" {};
        #   typst = self.callHackage "typst" "0.7" {};
        #   texmath = self.callHackage "texmath" "0.12.10" {};
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
            zlib
            ghcid
            cabal-install
            git
          ];
          inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
        };
        devShell = self.devShells.${system}.default;
      });
}
