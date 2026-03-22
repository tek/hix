{
  inputs.hix.url = "HIX";

  outputs = {hix, ...}: hix.lib._hix_test {
    packages = {
      foo = {
        src = ./.;
        library.enable = true;
        library.dependencies = ["bar"];
      };
      bar = {
        src = ./.;
        library.enable = true;
        library.dependencies = ["foo"];
      };
    };
  };
}
