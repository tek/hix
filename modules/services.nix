{config, lib, ...}:
with lib;
let

  serviceModule = import ./service.nix { global = config; inherit lib; };

  postgresModule = import ./postgres.nix { global = config; };

in {
  options = with types; {

    services = {

      postgres = mkOption {
        description = "PostgreSQL server";
        type = submoduleWith {
          modules = [serviceModule postgresModule];
          description = "submodule of service and postgres";
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
