# This flake is written using haskell-flake
# https://community.flake.parts/haskell-flake
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    # Remember to run `nix flake lock` after adding or modifying flake inputs
    # This will update the `flake.lock` file.
    typst-hs.url = "github:jgm/typst-hs/main";
    typst-hs.flake = false;
    typst-symbols.url = "github:jgm/typst-symbols/0.1.6";
    typst-symbols.flake = false;
    citeproc.url = "github:jgm/citeproc/0.8.1.1";
    citeproc.flake = false;
    commonmark-hs.url = "github:jgm/commonmark-hs/master";
    commonmark-hs.flake = false;
    emojis.url = "github:jgm/emojis/0.1.4.1";
    emojis.flake = false;
    skylighting.url = "github:jgm/skylighting/0.14.2";
    skylighting.flake = false;
    djot.url = "github:jgm/djoths/0.1.2.1";
    djot.flake = false;
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { config, self', pkgs, ... }: {
        haskellProjects.default = {
          # Haskell package overrides. See https://community.flake.parts/haskell-flake/dependency
          packages = {
            # Source overrides
            typst.source = inputs.typst-hs;
            typst-symbols.source = inputs.typst-symbols;
            citeproc.source = inputs.citeproc;
            commonmark-extensions.source = inputs.commonmark-hs + /commonmark-extensions;
            commonmark-pandoc.source = inputs.commonmark-hs + /commonmark-pandoc;
            commonmark.source = inputs.commonmark-hs + /commonmark;
            commonmark-cli.source = inputs.commonmark-hs + /commonmark-cli;
            emojis.source = inputs.emojis;
            djot.source = inputs.djot;

            # TODO: Move this to skylighting's flake, and import it here (as haskell-flake module)
            skylighting.source = pkgs.runCommand "skylighting" {} ''
              mkdir $out
              cp -r ${inputs.skylighting} $out/root
              chmod -R u+w $out
              cd $out/root/skylighting
              ${config.haskellProjects.default.outputs.finalPackages.skylighting-core}/bin/skylighting-extract ../skylighting-core/xml
              rm ./changelog.md; cp ../changelog.md .
              cd $out
              mv root/skylighting/* .
              rm -rf root
            '';
            skylighting-core.source = inputs.skylighting + /skylighting-core;
            skylighting-format-ansi.source = inputs.skylighting + /skylighting-format-ansi;
            skylighting-format-blaze-html.source = inputs.skylighting + /skylighting-format-blaze-html;
            skylighting-format-context.source = inputs.skylighting + /skylighting-format-context;
            skylighting-format-latex.source = inputs.skylighting + /skylighting-format-latex;

            # Hackage overrides
            texmath.source = "0.12.8.9";
            toml-parser.source = "2.0.0.0";
            tls.source = "2.0.5";
          };
          settings = {
            skylighting-core = {
              cabalFlags.executable = true;
            };
          };

          autoWire = [ "packages" "apps" "checks" ];

          # Programs you want to make available in the shell.
          # Default programs can be disabled by setting to 'null'
          devShell.tools = hp: {
            inherit (hp) hlint stylish-haskell;
          };
        };

        packages = {
          # This enable us to `nix build` or `nix run` pandoc.
          default = self'.packages.pandoc;
          # Tip: Run `nix shell .#skylighting-core` to enter a devShell with this executable in PATH
          skylighting-core = config.haskellProjects.default.outputs.finalPackages.skylighting-core;
        };

        devShells.default = pkgs.mkShell {
          name = "pandoc";
          packages = with pkgs; [
            # Add custom packages here
            bashInteractive
          ];
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
        };
      };
    };
}
