# based on https://gist.github.com/TikhonJelvis/be42400fc31bac0cd1736740fe5eb83b
{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  # Build a default.nix file from our .cabal file:
  here = ./.;
  project = pkgs.stdenv.mkDerivation ({
    name = "default.nix";

    buildCommand = ''
    ${pkgs.cabal2nix}/bin/cabal2nix file://${here} > $out
    '';
  });

  # Use the package set for our compiler:
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  # Helper function that gets Nix-packaged dependencies off GitHub.
  # GitHub project needs a default.nix file for this to work.
  fetchHaskell = { url, rev, sha256 }:
    haskellPackages.callPackage (pkgs.fetchgit { inherit url rev sha256; }) {};

  # Note: fetchHaskell shouldn't download the package if you already
  # have it in the system.

    base = haskellPackages.callPackage project {
    ## Specify GitHub dependencies here.
    ## You can get url, rev and sha256 by running 'nix-prefetch-git git@...'
    # extraPackage = fetchHaskell {
    #   url = "git@...";
    #   rev = "<commit hash>";
    #   sha256 = "<sha256 hash>";
    # };
  };

in

  if pkgs.lib.inNixShell then base.env else base
