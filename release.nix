let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.callPackage ./project.nix { }
