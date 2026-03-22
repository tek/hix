{
  inputs.hix.url = "HIX";

  outputs = {self, hix}: hix.lib._hix_test {
    inherit self;
    packages = {
      foo = { src = ./.; executable.source-dirs = "."; };
      bar = { src = ./.; };
    };
  };
}
