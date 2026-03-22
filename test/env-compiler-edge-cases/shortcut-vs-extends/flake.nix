{
  description = "Test: compiler shortcut vs package-set extends";
  inputs.hix.url = "HIX";
  outputs = {self, hix}: hix.lib._hix_test ({config, ...}: {
    inherit self;
    packages.root.src = ./.;

    outputs.legacyPackages = {
      version = config.envs.test-env.toolchain.version;
    };

    package-sets.my-ps = {
      compiler = "ghc98";
    };

    envs.test-env = {
      compiler = "ghc910";
      package-set.extends = "my-ps";
    };
  });
}
