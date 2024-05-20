{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = { hix, ... }: hix.lib.flake ({config, ...}: {
    packages = {
      core = {
        src = ./core;
        library.enable = true;
      };
      root = {
        src = ./pkg;
        library.enable = true;
        executable.enable = true;
        test.enable = true;
        library.dependencies = ["path" "path-io" "core" "ghc"];
      };
    };
    ghci = {
      run.print = ''putStrLn "print success"'';
      run.cwd = ''putStrLn . toFilePath =<< getCurrentDir'';
      setup.cwd = ''
      import Path (toFilePath)
      import Path.IO (getCurrentDir)
      '';
      args = ["-package ghc"];
    };
    commands.ghci-app = {
      ghci = {
        enable = true;
        runner = "print";
      };
      expose = true;
    };
    internal.hixCli.dev = false;
  });
}
