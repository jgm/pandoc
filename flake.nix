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

    # WASM toolchain, used (in the dev shell) to build pandoc for wasm32.
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";

    # Git dependency pinned in cabal.project's source-repository-package.
    texmath = {
      url = "github:jgm/texmath/170899673ee31de9096e178605e8da31a36e4185";
      flake = false;
    };
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, lib, system, ... }:
        let
          # Hackage versions that nixpkgs ships too old (or, for asciidoc, not
          # at all) for pandoc 3.10's `build-depends` bounds. These are the
          # highest Hackage versions in range; pandoc 3.10 itself proves they
          # are mutually compatible.
          bumpedDeps = {
            typst = "0.10";
            typst-symbols = "0.2";
            citeproc = "0.13.0.1";
            commonmark = "0.3";
            commonmark-extensions = "0.2.7";
            commonmark-pandoc = "0.3";
            emojis = "0.1.5";
            djot = "0.1.4";
            pandoc-types = "1.23.1.2";
            asciidoc = "0.1.0.3";

            # The hslua 2.5 ecosystem (used by pandoc-lua-engine). The
            # sub-libraries version independently of the `hslua` umbrella.
            hslua = "2.5.0";
            hslua-core = "2.3.2.1";
            hslua-marshalling = "2.3.2";
            hslua-classes = "2.3.2";
            hslua-objectorientation = "2.5.0";
            hslua-packaging = "2.4.1";
            hslua-aeson = "2.3.2";
            hslua-typing = "0.2.0";
            hslua-module-path = "1.2.0";
            hslua-module-system = "1.3.0";
            hslua-module-text = "1.2.0";
            hslua-module-version = "1.2.0.1";
            hslua-module-zip = "1.2.1";
            hslua-module-doclayout = "1.2.1.1";
          };

          # The sub-packages' `COPYING.md` is a `../COPYING.md` symlink that
          # dangles once the sub-directory is isolated as a source, breaking
          # the `license-file` copy during install. Replace it with the real
          # file from the repo root.
          fixLicense = pkg: pkg.overrideAttrs (oa: {
            postPatch = (oa.postPatch or "") + ''
              rm -f COPYING.md
              cp ${inputs.self}/COPYING.md COPYING.md
            '';
          });
        in
        {
        haskellProjects.default = {
          # pandoc 3.10's dependencies (citeproc 0.13, typst 0.10, commonmark
          # 0.3, ...) are newer than the all-cabal-hashes snapshot pinned in
          # nixpkgs, so `callHackage` can't find them. Point the package set at
          # a recent all-cabal-hashes that contains them.
          basePackages = pkgs.haskellPackages.override {
            all-cabal-hashes = pkgs.fetchurl {
              url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/de045027bcffb533171385da3b1e97aa05ba7633.tar.gz";
              sha256 = "01pqcqyc0yab7d7xvs2bwqvy4rqi7arbpw4823hd93gqkl6yics7";
            };
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

            # Git dependency pinned in cabal.project's source-repository-package.
            texmath.source = inputs.texmath;
          } // lib.mapAttrs (_: version: { source = version; }) bumpedDeps;

          settings = {
            pandoc = {
              # Build the self-contained executable (mirrors cabal.project's
              # `+embed_data_files`), so the binary works without a data dir.
              cabalFlags.embed_data_files = true;
              # Skip the (slow) test suite and haddock in `nix build`.
              check = false;
              haddock = false;
            };
            # The sub-package `.cabal` files reference `license-file: COPYING.md`
            # via a `../COPYING.md` symlink that dangles once the sub-directory
            # is isolated as a source; building from sdist would fail on it. The
            # license file isn't needed to compile, so build from source directly.
            pandoc-cli = { check = false; haddock = false; buildFromSdist = false; custom = fixLicense; };
            pandoc-lua-engine = { check = false; haddock = false; buildFromSdist = false; custom = fixLicense; };
            pandoc-server = { check = false; haddock = false; buildFromSdist = false; custom = fixLicense; };

            # texmath (git) and the bumped Hackage deps below: skip tests/haddock
            # and relax stale version bounds against this GHC's boot libraries.
            texmath = { check = false; haddock = false; jailbreak = true; };
          } // lib.genAttrs (builtins.attrNames bumpedDeps)
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
