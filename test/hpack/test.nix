{ pkgs }:
let

  target = builtins.toFile "cabal-target" ''
  cabal-version: 2.0

  -- This file has been generated from package.yaml by hpack version 0.34.6.
  --
  -- see: https://github.com/sol/hpack

  name:           root
  version:        23
  author:         Author McCodeface
  maintainer:     Author McCodeface
  build-type:     Simple

  library
    exposed-modules:
        Root.Lib
    hs-source-dirs:
        src
    build-depends:
        aeson
      , base >=4 && <6
      , incipit ==5
    mixins:
        incipit hiding (Prelude)
    if false
      other-modules:
          Paths_root
      autogen-modules:
          Paths_root
    default-language: Haskell2010

  executable run
    main-is: Main.hs
    other-modules:
        Paths_root
    autogen-modules:
        Paths_root
    hs-source-dirs:
        app
    build-depends:
        base >=4 && <6
      , root
    default-language: Haskell2010
  '';

in {
  test = ''
    cd ./root
    nix flake update
    nix run .#hpack

    if ! diff ${target} root.cabal
    then
      fail "The generated Cabal file differs from the target."
    fi
  '';
}
