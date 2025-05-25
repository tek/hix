{
  description = "hix test project";

  inputs.hix.url = "HIX";

  outputs = {hix, ...}:
  hix.lib._hix_test {
    packages.root = {
      src = ./.;
      cabal.dependencies = ["text"];
      library.enable = true;
      executable = {
        enable = true;
        name = "exe";
        source-dirs = "app";
      };
      executables.alias = {
        enable = true;
        source-dirs = "app";
      };
    };
    envs.cross = {
      expose = true;
      package-set.cross = "musl64";
    };
    overrides = { hsLib, self, ... }: {
      static-override = hsLib.justStaticExecutables self.root;
    };
    output.extraPackages = ["static-override"];
  };
}
