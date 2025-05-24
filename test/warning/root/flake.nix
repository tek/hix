{
  description = "hix test project";

  inputs.hix.url = "HIX";

  outputs = { hix, ... }:
  hix.lib._hix_test ({config, ...}: {
    packages.root.src = ./.;

    outputs.legacyPackages = {
      package-subpath = config.packages.root.subpath;
      ghc-version = config.envs.dev.ghc.version;
      ghc-overrides = config.envs.ghc-overrides.ghc.overrides;
    };

    envs.duplicate = {
      compiler = "ghc910";
      package-set.compiler.source = "ghc98";
    };

    envs.unexposed = {
      expose = false;
    };

    envs.systems = {
      expose.shell = true;
      systems = ["other-system"];
    };

    envs.disabled.enable = false;

    envs.ghc-overrides.ghc.overrides = [];

  });
}
