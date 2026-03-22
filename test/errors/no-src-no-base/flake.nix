{
  inputs.hix.url = "HIX";

  outputs = {hix, ...}: hix.lib._hix_test {
    packages.foo = {};
  };
}
