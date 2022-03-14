{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.flake {
    base = ./.;
    main = "root";
    packages.root = ./.;
    compat.enable = false;
    hpack.packages.root =
    let
      base = "base >= 4 && < 6";
    in {
      name = "root";
      author = "Author McCodeface";
      version = 23;

      library = {
        source-dirs = "src";
        dependencies = [base "aeson" { name = "incipit"; version = 5; mixin = "hiding (Prelude)"; }];
        when = { condition = "false"; generated-other-modules = "Paths_root"; };
      };

      executables.run = {
        main = "Main.hs";
        source-dirs = "app";
        dependencies = [base "root"];
      };
    };
  };
}
