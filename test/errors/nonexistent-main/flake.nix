{
  inputs.hix.url = "HIX";

  outputs = {hix, ...}: hix.lib._hix_test {
    main = "nonexistent";
    packages = {
      foo = {
        src = ./.;
        library.enable = true;
      };
    };
  };
}
