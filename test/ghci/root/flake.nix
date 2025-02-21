{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = { hix, ... }: hix.lib._hix_test ({config, ...}: {
    packages = {

      core = {
        src = ./core;
        library.enable = true;
      };

      api = {
        src = ./pkg;
        library.enable = true;
        executable.enable = true;
        test.enable = true;
        library.dependencies = ["path" "path-io" "core" "ghc"];
      };

      root = {

        src = ./.;

        library = {
          enable = true;
          source-dirs = "lib";
        };

        executable = {
          enable = true;
          source-dirs = ".";
        };
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
