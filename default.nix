# This Nix file can be handy when working on the github version of pandoc.
#
# To build pandoc run:
#    nix-build
# To run a nix shell with everthing needed to build pandoc with cabal:
#    nix-shell
# To build pandoc for use on Linux and macOS systems without Nix:
#    nix-build -A patched
{ pkgs ?
    import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
      owner = "NixOS"; repo = "nixpkgs";
      rev = "1354099daf98b7a1f79e6c41ce6bfda5c40177ae";
      sha256 = "1dwnmxirzjrshlaan7cpag77y7nd09chrbakfx3c3f1lzsldbi97";
    }) {} }:
let haskellPackages = pkgs.haskellPackages;
    overrides = self: super: {
      hslua-module-text = pkgs.haskell.lib.dontCheck
            super.hslua-module-text;
      skylighting = super.skylighting_0_5;
      hslua = super.hslua_0_9_3;
    };
    source-overrides = {
      doctemplates = "0.2.1";
      texmath = "0.10";
      pandoc-types = pkgs.fetchFromGitHub {
        owner = "jgm";
        repo = "pandoc-types";
        rev = "f1278603a4766f32b8375de84b8581f4bb1e665a";
        sha256 = "05ykcs8qdjrxly9b6chjr939mv6r42mh51bl58k7jxsr1crxrrf9";
      };
    };
    filterHaskellSource = src:
      builtins.filterSource (path: type:
        pkgs.lib.all (i: i != baseNameOf path) [ ".git" "dist-newstyle" "cabal.project.local" "dist" ".stack-work" ".DS_Store" "default.nix" "result" ]
          && pkgs.lib.all (i: !(pkgs.lib.hasSuffix i path)) [ ".lkshf" ]
          && pkgs.lib.all (i: !(pkgs.lib.hasPrefix i path)) [ ".ghc.environment." ]
        ) src;
    drv = (
        haskellPackages.extend (
          pkgs.lib.composeExtensions (
            haskellPackages.packageSourceOverrides source-overrides
          ) overrides
        )
      ).callCabal2nix "pandoc" (filterHaskellSource ./.) {};

    # Patch binaries for use on macOS and linux systems without nix
    # Linux version will still need GMP to be installed
    patched = pkgs.haskell.lib.justStaticExecutables (drv.overrideAttrs (old: {
        buildInputs = old.buildInputs ++ [
          pkgs.zip
          pkgs.gnutar
          pkgs.libiconv
          pkgs.zlib.static
          haskellPackages.file-embed
        ];
        configureFlags = [
          "-fembed_data_files"
          "--disable-executable-dynamic"
        ];
        postInstall = if pkgs.stdenv.isDarwin
          then ''
              cp ${pkgs.gmp}/lib/libgmp.10.dylib $out/bin
              chmod +w $out/bin/libgmp.10.dylib
              echo patching libgmp.10.dylib
              install_name_tool -id "@executable_path/libgmp.10.dylib" "$out/bin/libgmp.10.dylib"
              for fn in $out/bin/*; do
                echo patching $fn
                install_name_tool -change "${pkgs.libiconv}/lib/libiconv.dylib" /usr/lib/libiconv.dylib "$fn"
                install_name_tool -change "${pkgs.stdenv.libc}/lib/libSystem.B.dylib" /usr/lib/libSystem.B.dylib "$fn"
                install_name_tool -change "${pkgs.gmp}/lib/libgmp.10.dylib" "@executable_path/libgmp.10.dylib" "$fn"
              done
              (cd $out/.. && zip -r $out/pandoc-macOS.zip `basename $out`/bin)
            ''
          else ''
              for fn in $out/bin/*; do
                echo patching $fn
                patchelf --set-interpreter /lib64/ld-linux-x86-64.so.2 $fn
              done
              (cd $out/.. && tar -czf $out/pandoc-linux.tar.gz $out/bin)
            '';
      }
    ));
in if pkgs.lib.inNixShell then drv.env else drv // { inherit patched; }
