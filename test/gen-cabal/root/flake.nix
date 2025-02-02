{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = {hix, ...}: hix.lib._hix_test {

    managed.enable = false;
    managed.mergeBounds = true;

    packages.a = {
      src = ./a;
      library.enable = true;
    };

  };

}
