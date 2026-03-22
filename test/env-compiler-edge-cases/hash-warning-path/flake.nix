{
  description = "Test: hash warning config path when extending compiler with custom build";
  inputs.hix.url = "HIX";
  outputs = {self, hix}: hix.lib._hix_test ({config, ...}: {
    inherit self;
    packages.root.src = ./.;

    outputs.legacyPackages = {
      version = config.envs.test-env.toolchain.version;
    };

    compilers.build-base = {
      extends = null;
      source.build = {
        version = "9.10.1";
      };
    };

    compilers.extending-build = {
      extends = "build-base";
    };

    envs.test-env.compiler = "extending-build";
  });
}
