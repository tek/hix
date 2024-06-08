{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = {self, hix, ...}: hix.lib.flake ({config, lib, ...}: {
    managed = {
      enable = true;
      latest.compiler = "ghc94";
      envs.solverOverrides = {source, ...}: {
        multi-fail1 = source.root ./multi-fail-solve1;
        multi-fail2 = source.root ./multi-fail-solve2;
      };
    };
    envs.latest.overrides = {source, ...}: {
      multi-fail1 = source.root ./multi-fail1;
      multi-fail2 = source.root ./multi-fail2;
    };
    packages = {
      local1 = {
        src = ./packages/local1;
        library = {
          enable = true;
          dependencies = [
            "multi-fail1"
            "multi-fail2"
          ];
        };
      };
    };

    internal.hixCli.dev = true;

  });
}
