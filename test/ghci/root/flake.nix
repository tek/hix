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
      test.env = config.envs.test;
    };
    mainCompiler = "ghc902";
    envs.test = {
      services = [config.services.test];
    };
    services.test = {
      nixos.services.nginx = {
        enable = true;
        virtualHosts.localhost = {
          locations."/test".return = "200 test-endpoint";
        };
      };
      ports = [{ host = 2000; guest = 80; }];
    };
  });
}
