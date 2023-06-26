{
  description = "Pandoc development";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              pandoc = hfinal.callCabal2nix "pandoc" ./. { };
            };
        };
        pandoc = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.pandoc;
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShell = hspkgs.shellFor {
            withHoogle = true;
            packages = p : [
              p.pandoc
            ];
            buildInputs = [
              hspkgs.cabal-install
              hspkgs.haskell-language-server
              hspkgs.hlint
              hspkgs.ghcid
              hspkgs.ormolu
              hspkgs.stylish-haskell
              hspkgs.weeder
              hspkgs.servant-server
              hspkgs.hslua
              pkgs.bashInteractive
            ];
          };
          defaultPackage = pkgs.pandoc;
        };
    in
    { inherit overlay; } //
      inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
