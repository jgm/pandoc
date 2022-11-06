{nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    Diff
    Glob
    aeson
    aeson-pretty
    array
    attoparsec
    base
    base64
    binary
    blaze-html
    blaze-markup
    bytestring
    case-insensitive
    citeproc
    commonmark
    commonmark-extensions
    commonmark-pandoc
    connection
    containers
    data-default
    deepseq
    directory
    doclayout
    doctemplates
    emojis
    exceptions
    file-embed
    filepath
    Glob
    gridtables
    haddock-library
    hslua
    hslua-aeson
    hslua-module-doclayout
    hslua-module-path
    hslua-module-system
    hslua-module-text
    hslua-module-version
    http-client
    http-client-tls
    http-types
    ipynb
    jira-wiki-markup
    JuicyPixels
    lpeg
    mtl
    network
    network-uri
    pandoc-lua-marshal
    pandoc-types
    parsec
    pretty
    pretty-show
    process
    random
    safe
    scientific
    servant-server
    SHA
    skylighting
    skylighting-core
    split
    syb
    tagsoup
    tasty
    tasty-bench
    tasty-golden
    tasty-hunit
    tasty-lua
    tasty-quickcheck
    temporary
    texmath
    text
    text-conversions
    time
    unicode-collation
    unicode-transforms
    unix
    wai
    wai-extra
    warp
    xml
    xml-conduit
    xml-types
    yaml
    zip-archive
    zlib
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    pkgs.zlib
    ghc
    pkgs.gdb
    haskellPackages.ghcid
    haskellPackages.haskell-language-server
    haskellPackages.cabal2nix
    haskellPackages.cabal-install
    haskellPackages.hlint
    haskellPackages.stylish-haskell
  ];
in
pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = nixPackages;
}
