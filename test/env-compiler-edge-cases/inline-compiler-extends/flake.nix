{
  description = "Test: inline compiler in package-set extending a named compiler";
  inputs.hix.url = "HIX";
  outputs = {self, hix}: hix.lib._hix_test ({config, ...}: {
    inherit self;
    packages.root.src = ./.;

    outputs.legacyPackages = {
      # The inline compiler's name should include the package-set name as extender
      compiler-name = config.envs.test-env.package-set.compiler.name;
      # The inline compiler should use the source from the named compiler
      compiler-source = config.envs.test-env.package-set.compiler.source;
      # The tag should be inherited from the named compiler
      compiler-tag = config.envs.test-env.package-set.compiler.tag;
    };

    compilers.custom = {
      source = "ghc98";
      tag = "custom-tag";
    };

    # An env whose package-set has an inline compiler that extends a named one
    # and overrides the tag
    envs.test-env.package-set.compiler = {
      extends = "custom";
      tag = "overridden-tag";
    };
  });
}
