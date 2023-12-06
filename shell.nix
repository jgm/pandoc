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
    cassava
    commonmark
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
    hslua-aeson
    hslua-list
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
    lua
    lua-arbitrary
    lpeg
    mime-types
    mtl
    network
    network-uri
    Only
    ordered-containers
    pandoc-lua-marshal
    pandoc-types
    parsec
    pretty
    pretty-show
    process
    random
    regex-tdfa
    safe
    scientific
    servant-server
    SHA
    skylighting
    skylighting-core
    skylighting-format-latex
    skylighting-format-context
    skylighting-format-blaze-html
    skylighting-format-ansi
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
    wai-app-static
    wai-cors
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
