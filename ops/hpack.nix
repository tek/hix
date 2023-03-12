{ config, lib, ... }:
with builtins;
with lib;
let

  mergeAttr = a: b:
  if isAttrs a
  then merge a b
  else if isList a
  then a ++ b
  else b;

  merge = l: r:
  let
    f = name:
    if hasAttr name l && hasAttr name r
    then mergeAttr l.${name} r.${name}
    else l.${name} or r.${name};
  in genAttrs (concatMap attrNames [l r]) f;

  paths = name: {
    when = {
      condition = false;
      generated-other-modules = ["Paths_${replaceStrings ["-"] ["_"] name}"];
    };
  };

  meta = {
    version = import ./version.nix;
    license = "BSD-2-Clause-Patent";
    license-file = "LICENSE";
    author = "Torsten Schmits";
    maintainer = "hackage@tryp.io";
    copyright = "2023 Torsten Schmits";
    category = "Build";
    build-type = "Simple";
    git = "https://git.tryp.io/tek/hix";
    homepage = "https://git.tryp.io/tek/hix";
    bug-reports = "https://github.com/tek/hix/issues";
  };

  options.ghc-options = [
    "-Wall"
    "-Wredundant-constraints"
    "-Wincomplete-uni-patterns"
    "-Wmissing-deriving-strategies"
    "-Widentities"
    "-Wunused-packages"
    "-Wno-partial-type-signatures"
  ];

  dependencies = [
    { name = "base"; version = ">= 4.12 && < 5"; mixin = "hiding (Prelude)"; }
    { name = "incipit-base"; version = "^>= 0.5"; mixin = ["(IncipitBase as Prelude)" "hiding (IncipitBase)"]; }
  ];

  project = name: doc: merge (meta // { library = paths name; } // options) {
    inherit name;
    description = "See https://hackage.haskell.org/package/${name}/docs/${doc}.html";
    library = {
      source-dirs = "lib";
      inherit dependencies;
    };
    default-extensions = config.ghci.extensions;
  };

  exe = pkg: dir: merge (paths pkg // {
    main = "Main.hs";
    source-dirs = dir;
    dependencies = dependencies ++ [pkg];
    ghc-options = [
      "-threaded"
      "-rtsopts"
      "-with-rtsopts=-N"
    ];
  });

in {

  hix = merge (project "hix" "Hix") {
    synopsis = "Haskell Development Build Tools";
    library.dependencies = [
      "Cabal"
      "exon ^>= 1.4"
      "extra"
      "filepattern"
      "generic-lens"
      "lens"
      "lens-regex-pcre"
      "optparse-applicative"
      "random"
      "path"
      "transformers"
    ];

    tests.hix-unit = exe "hix" "test" {
      dependencies = [
        "Cabal"
        "exon ^>= 1.4"
        "hedgehog ^>= 1.1"
        "path"
        "path-io"
        "tasty ^>= 1.4"
        "tasty-hedgehog ^>= 1.3"
        "transformers"
      ];
    };

    executables.hix = exe "hix" "app" {
    };

  };


}
