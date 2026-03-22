{
  description = "Test: false positive duplicate compiler warning";
  inputs.hix.url = "HIX";
  outputs = {self, hix}: hix.lib._hix_test ({config, ...}: {
    inherit self;
    packages.root.src = ./.;

    outputs.legacyPackages = {
      version = config.envs.custom.toolchain.version;
    };

    # "ghc9101" is not in compilers.* (only ghc94, ghc96, ghc98, ghc910 from ghcVersions)
    # but IS a valid attribute in pkgs.haskell.packages.
    # The env module wraps it as {source = "ghc9101"} on the package-set compiler.
    # This produces a spurious "duplicate compiler" warning because the package-set
    # compiler also gets a default source from the extends chain.
    # The warning says "the former will be ignored" but the shortcut IS actually used.
    envs.custom.compiler = "ghc9101";
  });
}
