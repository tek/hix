{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.auto {
    packages.root = {
      src = ./.;
      executable = {
        enable = true;
        source-dirs = ".";
      };
    };
    compat.enable = false;
    envs.dev.ghc.compiler = "ghc902";
  };
}
