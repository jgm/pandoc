{nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    Diff
    Glob
    HTTP
    HTTP
    HsYAML
    JuicyPixels
    QuickCheck
    SHA
    aeson
    aeson-pretty
    attoparsec
    base
    base-compat
    base64-bytestring
    binary
    blaze-html
    blaze-markup
    bytestring
    case-insensitive
    citeproc
    commonmark
    commonmark-extensions
    commonmark-pandoc
    conduit-extra
    connection
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
    haddock-library
    hsc2hs
    hslua
    hslua-module-system
    hslua-module-text
    http-client
    http-client
    http-client-tls
    http-client-tls
    http-types
    ipynb
    jira-wiki-markup
    mtl
    network
    network
    network-uri
    pandoc-types
    parsec
    process
    random
    safe
    scientific
    skylighting
    skylighting-core
    socks
    split
    streaming-commons
    syb
    tagsoup
    tasty
    tasty-golden
    tasty-hunit
    tasty-lua
    tasty-quickcheck
    temporary
    texmath
    text
    text-conversions
    time
    tls
    unicode-transforms
    unordered-containers
    weigh
    xml
    xml-conduit
    zip-archive
    zlib
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    pkgs.zlib
    ghc
    pkgs.gdb
    haskellPackages.ghcid
    haskellPackages.cabal2nix
    haskellPackages.cabal-install
  ];
in
pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = nixPackages;
}
