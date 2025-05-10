{
  description = "hix test project";

  inputs.hix.url = "HIX";

  outputs = {hix, ...}:
  hix.lib._hix_test {

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

    internal.hixCli.dev = false;

  };
}
