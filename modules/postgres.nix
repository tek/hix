{global, ...}:
{config, lib, ...}:
with lib;
{
  options = with types; {

    package = mkOption {
      description = mdDoc "The postgresql package to use.";
      type = package;
      default = global.pkgs.postgresql_13;
    };

    name = mkOption {
      description = mdDoc "Database name.";
      type = str;
    };

    port = mkOption {
      description = mdDoc "Port on which to listen in the host system, added to the environment's `basePort`.";
      type = port;
      default = 32;
    };

    creds = {
      user = mkOption {
        description = mdDoc "Database user, defaulting to the database name.";
        type = str;
        default = config.name;
      };
      password = mkOption {
        description = mdDoc "Password for the user, defaulting to the user name.";
        type = str;
        default = config.creds.user;
      };
    };

    log = mkEnableOption "logging";

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
        initialScript = global.internal.pkgs.writeText "vm-postgresql-init" ''
        create database ${config.name};
        create role "${config.creds.user}" with login password '${config.creds.password}' createdb;
        grant all privileges on database "${config.name}" to "${config.creds.user}";
        '';
        settings = if config.log then { log_statement = "all"; log_min_messages = "info"; } else {};
      };
    };
    messages = env: ["Starting PostgreSQL on port ${toString env.hostPorts.postgres}"];
  };
}
