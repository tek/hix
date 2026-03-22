{
  description = "Test: env compiler shortcut referencing an existing compiler";
  inputs.hix.url = "HIX";
  outputs = {self, hix}: hix.lib._hix_test ({config, ...}: {
    inherit self;
    packages.root.src = ./.;

    outputs.legacyPackages = {
      version = config.envs.test-env.toolchain.version;
    };

    # ghc98 exists in compilers.* (from ghcVersionCompilers).
    # The shortcut passes it as a string ref, not an inline {source = ...}.
    # This should work cleanly with no warnings.
    envs.test-env.compiler = "ghc98";
  });
}
