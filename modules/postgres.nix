{global, ...}:
{config, lib, ...}: let
  inherit (lib) types;
in
{
  options = {

    package = lib.mkOption {
      description = "The postgresql package to use.";
      type = types.package;
      default = global.pkgs.postgresql_13;
    };

    name = lib.mkOption {
      description = "Database name.";
      type = types.str;
    };

    port = lib.mkOption {
      description = "Port on which to listen in the host system, added to the environment's `basePort`.";
      type = types.port;
      default = 32;
    };

    creds = {
      user = lib.mkOption {
        description = "Database user, defaulting to the database name.";
        type = types.str;
        default = config.name;
      };
      password = lib.mkOption {
        description = "Password for the user, defaulting to the user name.";
        type = types.str;
        default = config.creds.user;
      };
    };

    log = lib.mkEnableOption "logging";

  };

  config = {
    ports.postgres = { guest = 5432; host = config.port; };
    nixos-base = {
      services.postgresql = {
        enable = true;
        package = config.package;
        enableTCPIP = true;
        authentication = ''
        host all all 0.0.0.0/0 md5
        host all all ::/0 md5
        '';
        initialScript = global.pkgs.writeText "vm-postgresql-init" ''
        create database "${config.name}";
        create role "${config.creds.user}" with login password '${config.creds.password}' createdb;
        grant all privileges on database "${config.name}" to "${config.creds.user}";
        '';
        settings = if config.log then { log_statement = "all"; log_min_messages = "info"; } else {};
      };
    };
    messages = env: ["Starting PostgreSQL on port ${toString env.hostPorts.postgres}"];
  };
}
