{

  hix = {
    src = ../packages/hix;

    cabal = {

      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      version = import ./version.nix;
      author = "Torsten Schmits";

      meta = {
        maintainer = "hackage@tryp.io";
        category = "Build";
        git = "https://git.tryp.io/tek/hix";
        homepage = "https://git.tryp.io/tek/hix";
        bug-reports = "https://github.com/tek/hix/issues";
        synopsis = "Haskell/Nix development build tools";
      };

    };

    library.enable = true;
    library.dependencies = [
      "Cabal"
      "aeson ^>= 2.0"
      "exon ^>= 1.4"
      "extra ^>= 1.7"
      "filepattern ^>= 0.1"
      "generic-lens ^>= 2.2"
      "lens ^>= 5.1"
      "lens-regex-pcre ^>= 1.1"
      "optparse-applicative ^>= 0.17"
      "path ^>= 0.9"
      "path-io ^>= 1.7"
      "random ^>= 1.2"
      "transformers"
      "unix"
    ];

    executable.enable = true;

    test.enable = true;
    test.dependencies = [
      "Cabal"
      "exon ^>= 1.4"
      "hedgehog ^>= 1.1"
      "path ^>= 0.9"
      "path-io ^>= 1.7"
      "tasty ^>= 1.4"
      "tasty-hedgehog ^>= 1.3"
      "transformers"
    ];

  };

}
