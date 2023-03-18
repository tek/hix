{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.auto {
    packages.root = {
      src = ./.;
      library = { enable = true; source-dirs = "src"; };
      executable.enable = true;
    };
    compat.enable = false;
    devGhc.compiler = "ghc902";
  };
}
