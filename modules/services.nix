{config, lib, ...}:
let
  inherit (lib) types;

  postgresModule = import ./postgres.nix { global = config; };

in {
  options = {

    services = lib.mkOption {
      description = ''
      Services are fragments of NixOS config that can be added to an environment to be started as virtual machines when
      the environment is used in a command or shell.
      '';
      type = types.attrsOf types.deferredModule;
      default = {};
    };

    internal.services = {

      postgres = lib.mkOption {
        description = "A PostgreSQL server.";
        type = types.deferredModule;
        readOnly = true;
      };

    };

  };

  config.services = {

    hix-internal-env-wait = {
      ports.hix-internal-env-wait = { guest = 15000; host = 1; };
      nixos.systemd.services.hix-wait = {
        wantedBy = ["multi-user.target"];
        serviceConfig.ExecStart = "${config.pkgs.socat}/bin/socat TCP-L:15000,fork SYSTEM:'echo running'";
      };
    };

    ssh = {
      ports.ssh = { guest = 22; host = 22; };
      nixos = {
        services.openssh = {
          enable = true;
          settings.PermitRootLogin = "yes";
        };
        users.mutableUsers = true;
        users.users.root.password = "";
      };
    };

  };

  config.internal.services = {
    postgres = postgresModule;
  };

}
