{
  inputs.hix.url = "HIX";

  outputs = {self, hix}: hix.lib._hix_test {
    inherit self;
    packages.root.executable.source-dirs = ".";
  };
}
