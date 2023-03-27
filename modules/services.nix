{config, lib, ...}:
with lib;
let

  serviceModule = import ./service.nix { global = config; inherit lib; };

  postgresModule = import ./postgres.nix { global = config; };

in {
  options = with types; {

    services = mkOption {
      description = "Services";
      type = attrsOf (submodule serviceModule);
      default = {};
    };

    service-configs = {

      postgres = mkOption {
        description = "PostgreSQL server";
        type = submoduleWith {
          modules = [postgresModule];
          description = "Postgres submodule";
        };
        default = {};
      };

    };

  };

  config.services = {

    postgres = {
      nixos-base = {

      };
    };

  };
}
