{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = { hix, ... }: hix.lib._hix_test {
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

        cabal.prelude = {
          enable = true;
          package = {
            name = "incipit-core";
            version = ">= 0.4 && < 0.6";
          };
          module = "IncipitCore";
        };

        library = {
          enable = true;
          dependencies = [{ name = "aeson"; version = "^>= 2.1"; } "array"];
        };

      };

    };

  };
}
