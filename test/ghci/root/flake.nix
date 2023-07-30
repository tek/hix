{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }: hix.lib.flake ({config, ...}: {
    packages.root = {
      src = ./pkg;
      library.enable = true;
      library.dependencies = ["http-client" "bytestring" "path" "path-io"];
      library.default-extensions = ["OverloadedStrings"];
      executable.enable = true;
      test.enable = true;
      test.env = "hix-ghci-test";
    };
    envs.hix-ghci-test.services.test.enable = true;
    services.test = {
      nixos.services.nginx = {
        enable = true;
        virtualHosts.localhost.locations."/test".return = "200 test-endpoint";
      };
      ports.nginx = { host = 2; guest = 80; };
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
