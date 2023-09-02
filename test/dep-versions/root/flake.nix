{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = { hix, ... }:
  hix.lib.flake {
    main = "root";
    packages = {

      dep = {
        src = ./dep;
        cabal = {
          dependencies = ["aeson >= 2" "extra ^>= 1.7"];
        };
        library = {
          enable = true;
          dependencies = ["uuid == 1.3" "vector >= 0.11 && < 0.13"];
        };
      };

      root = {
        src = ./.;

        library = {
          enable = true;
          dependencies = [{ name = "aeson"; version = "^>= 2.1"; } "array"];
        };

      };

    };

  };
}
