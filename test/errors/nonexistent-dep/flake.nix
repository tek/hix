{
  description = "Test: dependency on nonexistent package";
  inputs.hix.url = "HIX";
  outputs = {self, hix}: hix.lib._hix_test {
    inherit self;
    packages.root = {
      src = ./.;
      library = {
        enable = true;
        dependencies = ["nonexistent-package-xyz"];
      };
    };
  };
}
