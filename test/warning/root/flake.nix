{
  description = "hix test project";

  inputs.hix.url = "HIX";

  outputs = { hix, ... }:
  hix.lib._hix_test ({config, ...}: {
    packages.root.src = ./.;

    outputs.legacyPackages = {
      package-subpath = config.packages.root.subpath;
      ghc-version = config.envs.dev.ghc.version;
    };

    envs.duplicate = {
      compiler = "ghc910";
      package-set.compiler.source = "ghc98";
    };

  });
}
