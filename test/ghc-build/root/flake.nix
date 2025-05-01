{
  description = "hix test project";

  inputs.hix.url = "HIX";

  outputs = {hix, ...}:
  hix.lib._hix_test {
    packages.root.src = ./.;

    envs.dev = {
      profiling = false;

      overrides = {noprofiling, noshared, ...}: {
        __all = noprofiling noshared;
      };

      package-set.compiler.source.build = {
        version = "9.10.1";
        hash = "sha256-vzhqMC1O4FR5H/1RdIkA8V1xdg/RmRV5ItEgzB+J4vc=";
        flavour = "quickest";
        builderArgs.enableShared = false;
      };

    };
  };
}
