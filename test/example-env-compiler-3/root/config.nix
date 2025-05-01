{
  compilers.my-ghc = {
    source.build = {
      version = "9.10.1";
      hash = "sha256-vzhqMC1O4FR5H/1RdIkA8V1xdg/RmRV5ItEgzB+J4vc=";
      flavour = "quickest";
      builderArgs.enableShared = false;
    };
  };

  envs.quickest.package-set = {
    compiler = "my-ghc";
  };

  envs.quick.package-set = {
    compiler = {
      extends = "my-ghc";
      source.build.flavour = "quick";
    };
  };
}
