{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = { hix, ... }:
  hix.lib._hix_test {
    packages.root = {
      src = builtins.path { path = ./.; filter = (_: _: true); };
      cabal.dependencies = ["aeson"];
      library.enable = true;
      library.dependencies = ["messagepack"];
      executables.root = { enable = true; source-dirs = "."; };
      test.enable = true;
    };
    cabal.dependencies = ["base" "some"];
    envs.dev.haskellTools = g: [g.fourmolu];
    envs.dev.overrides = {hackage, ...}: {
      fourmolu = hackage "0.14.0.0" "1bhfx6j79hc32q22c7dagirby5j5dhydm0cm9av7dhhjd71rr2xk";
    };
  };
}
