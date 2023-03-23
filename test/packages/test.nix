{ pkgs }:
let

  targetDep = builtins.toFile "cabal-target-dep" ''
  cabal-version: 1.12

  -- This file has been generated from package.yaml by hpack version 0.35.0.
  --
  -- see: https://github.com/sol/hpack

  name:           dep
  version:        13
  description:    See https://hackage.haskell.org/package/dep/docs/Dep.html
  author:         Author McCodeface
  maintainer:     Author McCodeface
  license:        GPL-3
  build-type:     Simple

  library
    exposed-modules:
        Dep.Lib
    other-modules:
        Paths_dep
    hs-source-dirs:
        lib
    default-extensions:
        OverloadedRecordDot
        DataKinds
    ghc-options: -Wall -Werror -Wunused-imports
    build-depends:
        base >=4 && <6
    default-language: GHC2021
  '';

  targetRoot = builtins.toFile "cabal-target-root" ''
  cabal-version: 2.0

  -- This file has been generated from package.yaml by hpack version 0.35.0.
  --
  -- see: https://github.com/sol/hpack

  name:           root
  version:        23
  description:    See https://hackage.haskell.org/package/root/docs/Root.html
  author:         Author McCodeface
  maintainer:     Author McCodeface
  license:        GPL-3
  build-type:     Simple

  library
    exposed-modules:
        Root.Lib
    hs-source-dirs:
        src
    default-extensions:
        DataKinds
    ghc-options: -Wunused-imports
    build-depends:
        aeson
      , base >=4 && <6
      , incipit ==5
    mixins:
        incipit hiding (Prelude)
    default-language: GHC2021

  executable run
    main-is: Run.hs
    other-modules:
        Paths_root
    autogen-modules:
        Paths_root
    hs-source-dirs:
        app
    default-extensions:
        DataKinds
    ghc-options: -threaded -rtsopts -with-rtsopts=-N -Wunused-imports
    build-depends:
        base >=4 && <6
      , dep
      , root
    default-language: GHC2021

  test-suite root-unit
    type: exitcode-stdio-1.0
    main-is: Main.hs
    other-modules:
        Paths_root
    autogen-modules:
        Paths_root
    hs-source-dirs:
        test
    default-extensions:
        DataKinds
    ghc-options: -threaded -rtsopts -with-rtsopts=-N -Wunused-imports
    build-depends:
        base >=4 && <6
      , root
    default-language: GHC2021
  '';

in {
  test = builtins.toFile "packages-test" ''
    cd ./root
    nix flake update
    nix run .#gen-cabal-quiet

    if ! diff ${targetDep} dep/dep.cabal
    then
      fail "The generated Cabal file for 'dep' differs from the target."
    fi

    if ! diff ${targetRoot} root.cabal
    then
      fail "The generated Cabal file for 'root' differs from the target."
    fi
  '';
}
