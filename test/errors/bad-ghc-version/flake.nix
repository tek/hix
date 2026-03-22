{
  description = "Test: nonexistent GHC version";
  inputs.hix.url = "HIX";
  outputs = {self, hix}: hix.lib._hix_test {
    inherit self;
    packages.root.src = ./.;
    envs.bad.compiler = "ghc999";
  };
}
