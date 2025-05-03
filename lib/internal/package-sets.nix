{util}: let

  toToolchain = packageSet: let

    inherit (packageSet.pkgs) __hix;
  in {
    inherit (packageSet)
    pkgs
    overrides
    tag
    conf
    ;

    inherit (__hix)
    compiler
    packages
    vanilla
    ;

    # Not using `compiler.version` here to avoid evaluating that expression unnecessarily.
    version = __hix.packages.ghc.version;

  };

in {
  inherit toToolchain;
}
