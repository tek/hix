{
  description = "hix test project";

  inputs.hix.url = "path:HIX";

  outputs = { hix, ... }: hix.lib._hix_test ({config, ...}: {

    name = "hix-test-service";

    services.test = {
      enable = true;
      ports.test = { guest = 5000; host = 5; };
      nixos = {
        systemd.services.test = {
          wantedBy = ["multi-user.target"];
          serviceConfig = {
            ExecStart = "${config.pkgs.socat}/bin/socat TCP-L:5000,fork SYSTEM:'echo received'";
          };
        };
      };
    };

    services.disabled-global = {
      enable = false;
      nixos = throw "nixos config of disabled service should not be evaluated";
    };

    services.disabled-local = {
      enable = true;
      nixos = throw "nixos config of disabled service should not be evaluated";
    };

    envs.test = {
      basePort = 15000;
      services.test.enable = true;
    };

    envs.disabled-global = {
      services.disabled-global.enable = true;
    };

    envs.disabled-local = {
      services.disabled-local.enable = false;
    };

    commands.test = {
      env = "test";
      command = ''
      ${config.pkgs.socat}/bin/socat -T 1 - TCP:localhost:${toString (config.envs.test.hostPorts.test)} <<< 'ping'
      '';
    };

    commands.disabled-global = {
      env = "disabled-global";
      command = "echo done";
    };

    commands.disabled-local = {
      env = "disabled-local";
      command = "echo done";
    };

    envs.db = {
      basePort = 16000;
      services.postgres = {
        enable = true;
      };
    };

    commands.db-test = {
      env = "db";
      command = ''
      '';
    };

  });
}
