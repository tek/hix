{
  description = "Test: custom GHC build without version";
  inputs.hix.url = "HIX";
  outputs = {self, hix}: hix.lib._hix_test {
    inherit self;
    packages.root.src = ./.;
    compilers.custom.source.build = {};
    envs.custom.compiler = "custom";
  };
}
