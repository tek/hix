{
  compilers.my-ghc = {
    source = "ghc910";
  };

  envs.dev.package-set = {
    compiler = "my-ghc";
  };
}
