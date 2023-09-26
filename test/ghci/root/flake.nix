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
        library.dependencies = ["path" "path-io" "core"];
        # library.default-extensions = ["OverloadedStrings"];
      };
    };
    ghci.run.print = ''putStrLn "print success"'';
    ghci.run.cwd = ''putStrLn . toFilePath =<< getCurrentDir'';
    ghci.setup.cwd = ''
    import Path (toFilePath)
    import Path.IO (getCurrentDir)
    '';
    commands.ghci-app = {
      ghci = {
        enable = true;
        runner = "print";
      };
      expose = true;
    };
    internal.hixCli.dev = true;
  });
}
