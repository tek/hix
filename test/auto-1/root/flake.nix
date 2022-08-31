{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.auto {
    packages.root = ./.;
    compat.enable = false;
    devGhc.compiler = "ghc902";
    hpack.packages.root =
    let
      base = { name = "base"; version = ">= 4.12 && < 5"; };
    in {
      name = "root";
      author = "Author McCodeface";
      version = "23";
      license = "BSD-2-Clause-Patent";

      library = {
        source-dirs = "src";
        dependencies = ["transformers >= 0 && < 100" "aeson" base];
        when = { condition = "false"; generated-other-modules = "Paths_root"; };
      };

      executables.run = {
        main = "Main.hs";
        source-dirs = "app";
        dependencies = [base "root" "polysemy"];
      };
    };
  };
}
