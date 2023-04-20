{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }: hix.lib.auto {
    cabal = {
      base = { name = "base"; version = ">= 4.12 && < 5"; };
      version = "23";
      license = "GPL-3";
      meta.author = "Author McCodeface";
    };
    packages.root = {
      src = ./.;

      library = {
        enable = true;
        source-dirs = "src";
        dependencies = ["transformers >= 0 && < 100" "aeson"];
        paths = false;
      };

      executables.run = {
        source-dirs = "app";
        dependencies = ["polysemy"];
      };

      buildInputs = pkgs: [pkgs.socat];

    };
    compat.enable = false;
    envs.dev.ghc.compiler = "ghc90";
    overrides = {self, notest, ...}: {
      root1 = notest self.root;
    };
    output.extraPackages = ["root1"];
  };
}
