{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }: hix.lib.flake ({config, ...}: let

  testService = {
    enable = true;
    ports = [{ guest = 5000; host = 5; }];
    nixos = {
      systemd.services.test = {
        wantedBy = ["multi-user.target"];
        serviceConfig = {
          ExecStart = "${config.pkgs.socat}/bin/socat TCP-L:5000,fork SYSTEM:'echo received'";
        };
      };
    };
  };

  in {

    envs.test = {
      basePort = 15000;
      services = [testService];
    };

    commands.test = {
      env = config.envs.test;
      command = ''
      ${config.pkgs.socat}/bin/socat -T 1 - TCP:localhost:${toString (config.envs.test.basePort + 5)} <<< 'ping'
      '';
    };

  });
}
