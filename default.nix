{ pkgs ?
    import (builtins.fetchTarball
    	  "https://github.com/NixOS/nixpkgs/archive/561de381bcccfe6792f2908a5022449a05ae0050.tar.gz"
    	  ) {} }:
  pkgs.haskellPackages.developPackage {
    overrides = self: super: {
      hslua-module-text = pkgs.haskell.lib.dontCheck
            super.hslua-module-text;
      tagsoup = super.tagsoup_0_14_2;
      skylighting = super.skylighting_0_4_4_1;
    };
    source-overrides = {
      hslua = "0.9.2";
      doctemplates = "0.2.1";
      texmath = "0.10";
      pandoc-types = pkgs.fetchFromGitHub {
        owner = "jgm";
        repo = "pandoc-types";
        rev = "f1278603a4766f32b8375de84b8581f4bb1e665a";
        sha256 = "05ykcs8qdjrxly9b6chjr939mv6r42mh51bl58k7jxsr1crxrrf9";
      };
    };

    root = ./.;
}
