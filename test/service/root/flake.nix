{
  description = "hix test project";

  inputs.hix.url = path:HIX;

  outputs = { hix, ... }: hix.lib.flake ({config, ...}: let

  in {

    services.test = {
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

    envs.test = {
      basePort = 15000;
      services.test.enable = true;
    };

    commands.test = {
      env = config.envs.test;
      command = ''
      ${config.pkgs.socat}/bin/socat -T 1 - TCP:localhost:${toString (config.envs.test.basePort + 5)} <<< 'ping'
      '';
    };

  });
}
