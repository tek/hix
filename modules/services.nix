{config, lib, ...}:
with lib;
let

  serviceModule = import ./service.nix { global = config; inherit lib; };

  postgresModule = import ./postgres.nix { global = config; };

  waitServiceModule = {
    options = with types; {

      wait = mkOption {
        description = "Wait for the VM to complete startup within the given number of seconds.";
        type = int;
        default = 30;
      };

    };
  };

in {
  options = with types; {

    services = mkOption {
      description = "Services";
      type = attrsOf unspecified;
      default = {};
    };

    service = {

      hix-internal-env-wait = mkOption {
        description = "";
        type = deferredModule;
        default = waitServiceModule;
      };

      # postgres = mkOption {
      #   description = "PostgreSQL server";
      #   type = submoduleWith {
      #     modules = [postgresModule];
      #     description = "Postgres submodule";
      #   };
      #   default = {};
      # };

    };

  };

  config.services = {

    hix-internal-env-wait = {
      ports = [{ guest = 15000; host = 1; }];
      nixos.systemd.services.hix-wait = {
        wantedBy = ["multi-user.target"];
        serviceConfig = {
          ExecStart = "${config.pkgs.socat}/bin/socat TCP-L:15000,fork SYSTEM:'echo running'";
        };
      };
    };

    postgres = {
      nixos-base = {

      };
    };

  };
}
