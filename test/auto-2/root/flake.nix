{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }: hix.lib.flake ({config, ...}: {
    packages.root = {
      src = ./.;
      library = { enable = true; source-dirs = "src"; };
      executable.enable = true;
      buildInputs = [config.pkgs.socat];
    };
    compat.enable = false;
    envs.dev.ghc.compiler = "ghc90";
  });
}
