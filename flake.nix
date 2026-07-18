{
  description = "pandoc";

  # Uses haskell-flake (https://haskell.nixos.asia) to build the local Haskell
  # packages (pandoc, pandoc-cli, pandoc-lua-engine, pandoc-server) defined in
  # cabal.project.
  #
  #   nix build   # builds the `pandoc` executable (from pandoc-cli)
  #   nix run     # runs it
  #   nix develop # dev shell (cabal, HLS, the wasm toolchain, etc.)
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    # pandoc 3.10's dependencies (citeproc 0.13, typst 0.10, commonmark
    # 0.3, ...) are newer than the all-cabal-hashes snapshot pinned in
    # nixpkgs, so `callHackage` can't find them. Point the package set at
    # a recent all-cabal-hashes that contains them.
    all-cabal-hashes = {
      url = "github:commercialhaskell/all-cabal-hashes/hackage";
      flake = false;
    };
    # WASM toolchain, used (in the dev shell) to build pandoc for wasm32.
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, lib, system, ... }:
        let
          # Single source of truth for dependency versions: pandoc's own
          # `stack.yaml` `extra-deps`. haskell-flake's package set lags pandoc's
          # `build-depends`, so we apply the very same overrides stack does.
          # Parsed at eval time (IFD: yaml -> json).
          stackExtraDeps = builtins.fromJSON (builtins.readFile
            (pkgs.runCommand "stack-extra-deps.json" { } ''
              ${pkgs.yq-go}/bin/yq -o=json '.extra-deps' \
                ${inputs.self + "/stack.yaml"} > $out
            ''));

          # "hslua-module-doclayout-1.2.1.1" -> name + version
          # (Hackage versions contain no '-', so version = last '-'-segment).
          parseDep = s:
            let parts = lib.splitString "-" s;
            in lib.nameValuePair
              (lib.concatStringsSep "-" (lib.init parts))
              (lib.last parts);

          # name -> version, derived from stack.yaml's plain `pkg-version` entries.
          bumpedDeps =
            lib.listToAttrs
              (map parseDep (builtins.filter builtins.isString stackExtraDeps))
            // {
              # Add things here that go beyond what's in stack.yaml, if needed:
              # hslua-core = "2.3.2.1";
            };

          # git `source-repository-package`s from stack.yaml.
          # The package name is read from the .cabal file at the source
          # root (the repo basename can differ, e.g. typst-hs -> typst).
          cabalPackageName = dir:
            let
              cabalFiles = lib.filterAttrs
                (n: t: t == "regular" && lib.hasSuffix ".cabal" n)
                (builtins.readDir dir);
            in
            lib.removeSuffix ".cabal" (lib.head (builtins.attrNames cabalFiles));

          gitSources = lib.listToAttrs (lib.concatMap
            (d:
              let
                repo = builtins.fetchGit {
                  url = d.git;
                  rev = d.commit;
                  allRefs = true;
                };
                srcDirs =
                  if d ? subdirs
                  then map (sub: repo + "/${sub}") d.subdirs
                  else [ repo ];
              in
              map
                (dir: lib.nameValuePair
                  (cabalPackageName dir)
                  { source = dir; })
                srcDirs)
            (builtins.filter builtins.isAttrs stackExtraDeps));

        in
        {
        haskellProjects.default = {
          basePackages = pkgs.haskellPackages.override {
            all-cabal-hashes = inputs.all-cabal-hashes;
          };

          # haskell-flake's cabal.project parser can't handle pandoc's
          # `if arch(wasm32)` conditional blocks, so disable auto-discovery
          # and list the packages explicitly. Sub-package sources are derived
          # from the flake root (`self`) so they are recognized as local
          # (i.e. under `projectRoot`).
          defaults.packages = lib.mkForce { };

          packages = {
            # Local packages (from cabal.project).
            pandoc.source = inputs.self;
            pandoc-cli.source = inputs.self + "/pandoc-cli";
            pandoc-lua-engine.source = inputs.self + "/pandoc-lua-engine";
            pandoc-server.source = inputs.self + "/pandoc-server";
          }
          // gitSources
          // lib.mapAttrs (_: version: { source = version; }) bumpedDeps;

          settings = {
            pandoc = {
              # Set flags (cabal.project is ignored):
              cabalFlags.embed_data_files = true;
              cabalFlags.http = true;
              # Skip the (slow) test suite and haddock in `nix build`.
              check = false;
              haddock = false;
            };
            pandoc-cli = {
              check = false;
              haddock = false;
              buildFromSdist = true;
            };
            pandoc-lua-engine = {
              check = false;
              haddock = false;
              buildFromSdist = true;
            };
            pandoc-server = {
              check = false;
              haddock = false;
              buildFromSdist = true;
            };

            # The git sources (texmath) and the bumped Hackage deps: skip
            # tests/haddock and relax stale bounds against this GHC's boot libs.
          } // lib.genAttrs
            (builtins.attrNames bumpedDeps ++ builtins.attrNames gitSources)
            (_: { check = false; haddock = false; jailbreak = true; });

          # Dev shell (`nix develop`). haskell-flake already provides
          # cabal-install, haskell-language-server, ghcid and hlint.
          devShell = {
            tools = hp: {
              inherit (hp)
                cabal-plan
                hpc
                stylish-haskell
                eventlog2html
                profiterole
                profiteur;
            };
            mkShellArgs.nativeBuildInputs =
              [ inputs.ghc-wasm-meta.packages.${system}.default ]
              ++ (with pkgs; [
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
              ]);
          };

          autoWire = [ "packages" "apps" "devShells" ];
        };

        # `nix build` / `nix run` -> the `pandoc` executable from pandoc-cli.
        packages.default = self'.packages.pandoc-cli;
      };
    };
}
