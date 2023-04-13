{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.flake ({config, ...}: {
    packages.root = {
      src = ./.;
      library.enable = true;
      library.dependencies = ["http-client" "bytestring"];
      library.default-extensions = ["OverloadedStrings"];
      executables.root = { enable = true; source-dirs = "app"; };
      test.enable = true;
      test.env = "hix-ghci-test";
    };
    compiler = "ghc90";
    envs.hix-ghci-test.services.test.enable = true;
    services.test = {
      nixos.services.nginx = {
        enable = true;
        virtualHosts.localhost.locations."/test".return = "200 test-endpoint";
      };
      ports.nginx = { host = 2; guest = 80; };
    };
    ghci.run.print = ''putStrLn "print success"'';
    internal.hixCli.dev = true;
  });
}
