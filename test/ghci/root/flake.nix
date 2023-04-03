{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }:
  hix.lib.flake ({config, ...}: {
    packages.root = {
      src = ./.;
      library.enable = true;
      library.dependencies = ["http-client" "bytestring"];
      executables.root = { enable = true; source-dirs = "app"; };
      test.enable = true;
      test.env = "test";
    };
    compiler = "ghc902";
    envs.test.services.test.enable = true;
    services.test = {
      nixos.services.nginx = {
        enable = true;
        virtualHosts.localhost = {
          locations."/test".return = "200 test-endpoint";
        };
      };
      ports = [{ host = 2000; guest = 80; }];
    };
    ghci.run.print = ''putStrLn "print success"'';
  });
}
