{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = { hix, ... }: hix.lib._hix_test {
    main = "root";
    packages = {
      prel = {
        src = ./packages/prel;
        library.enable = true;
      };
      root = {
        src = ./packages/root;
        cabal.prelude = {
          enable = true;
          package = "prel";
          module = "Stuff";
        };
      };
    };
  };
}
