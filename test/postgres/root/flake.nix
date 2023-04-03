{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }: hix.lib.flake ({config, ...}: {

    envs.db = {
      basePort = 16000;
      services.postgres = {
        enable = true;
      };
      vm.name = "hix-test-db";
    };

    commands.db-test = {
      env = "db";
      command = ''
      psql "host=localhost port=16032 user=test password=test dbname=test" -c 'select 1'
      '';
    };

    services.postgres = {
      name = "test";
      port = 32;
    };

  });
}
