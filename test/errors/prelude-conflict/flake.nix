{
  description = "Test: prelude package with conflicting bounds in dependencies";
  inputs.hix.url = "HIX";
  outputs = {self, hix}: hix.lib._hix_test {
    inherit self;
    packages.root = {
      src = ./.;
      library = {
        enable = true;
        prelude = {
          enable = true;
          package = { name = "base"; version = ">=4 && <5"; };
        };
        dependencies = [
          { name = "base"; version = ">=4.15"; }
        ];
      };
    };
  };
}
