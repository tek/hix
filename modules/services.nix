{config, lib, ...}:
with lib;
let

  postgresModule = import ./postgres.nix { global = config; };

in {
  options = with types; {

    services = mkOption {
      description = mdDoc ''
      Services are fragments of NixOS config that can be added to an environment to be started as virtual machines when
      the environment is used in a command or shell.
      '';
      type = attrsOf deferredModule;
      default = {};
    };

    internal.services = {

      postgres = mkOption {
        description = mdDoc "A PostgreSQL server.";
        type = deferredModule;
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

  };

  config.internal.services = {
    postgres = postgresModule;
  };
}
