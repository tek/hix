global:
cmdConf:
{ lib, config, ... }:
with lib;
with types;
let

  basicVmNixosConf = config: {
    virtualisation.vmVariant.virtualisation = {
      diskImage = config.image;
      diskSize = 4096;
      forwardPorts = [
        { host.port = config.port + 22; guest.port = 22; }
      ] ++ config.ports;
    };
    services.openssh = {
      enable = true;
      permitRootLogin = "yes";
    };
    users.mutableUsers = true;
    users.users.root.password = "";
    networking.firewall.enable = false;
  };

  postgresNixosConf = vm: {
    services.postgresql =
      let
        pkgs = global.internal.basicPkgs;
        cfg = vm.postgres;
        user = cfg.creds.user or name;
      in {
        enable = true;
        package = pkgs.postgresql_13;
        enableTCPIP = true;
        ensureDatabases = [cfg.name];
        authentication = ''
          host all all 0.0.0.0/0 md5
          host all all ::/0 md5
        '';
        initialScript = pkgs.writeText "vm-postgresql-init" ''
          create role "${user}" with login password '${cfg.creds.password or cfg.name}' createdb;
          grant all privileges on database "${cfg.name}" to "${user}";
        '';
        settings = if cfg.log then { log_statement = "all"; log_min_messages = "info"; } else {};
      };
  };

  postgresConf = vmConf: submodule ({ config, ... }: {
    options = {
      enable = mkEnableOption "a PostgreSQL server in this vm";

      name = mkOption {
        type = str;
      };

      port = mkOption {
        type = port;
      };

      creds = mkOption {
        type = submodule {
          options = {
            user = mkOption {
              type = str;
            };
            password = mkOption {
              type = str;
            };
          };

          config = {
            user = mkDefault config.name;
            password = mkDefault config.name;
          };
        };
        default = {};
      };

      log = mkEnableOption "logging";
    };

    config = {
      name = mkDefault vmConf.name;
      port = mkDefault vmConf.port;
    };
  });

in {
  options = {

    enable = mkEnableOption "a vm for this command";

    name = mkOption {
      type = str;
      default = "hix-vm";
      description = "Used for the temporary directory storing the image.";
    };

    dir = mkOption {
      type = str;
      description = "The directory in which the image will be stored.";
    };

    port = mkOption {
      type = port;
      description = "Port from which the `ssh` port is calculated (`+ 22`).";
      default = 10000;
    };

    ports = mkOption {
      type = listOf (attrsOf unspecified);
      description = "Additional port forwardings in the format expected by `virtualisation.forwardPorts`.";
      default = [];
    };

    postgres = mkOption {
      type = postgresConf config;
      description = "Config for running a PostgreSQL server in the VM.";
      default = {};
    };

    conf = mkOption {
      type = attrsOf unspecified;
      description = "Additional config merged into the basic NixOS config for the VM.";
      default = {};
    };

    pidfile = mkOption {
      type = str;
      description = "The file storing the qemu process' process ID.";
    };

    image = mkOption {
      type = str;
      description = "The path to the image file.";
    };

    headless = mkOption {
      type = bool;
      default = true;
      description = ''
      VMs are run without a graphical connection to their console.
      For debugging purposes, this option can be disabled to show the window.
      '';
    };

    vm = mkOption {
      type = unspecified;
      description = "The final derivation for the VM.";
    };

  };

  config = {
    dir = mkDefault "/tmp/hix-vm/$USER/${config.name}";

    ports = if config.postgres.enable then [{ host.port = config.port; guest.port = 5432; }] else [];

    conf =
      let
        basic = basicVmNixosConf config;
        pg = if config.postgres.enable then postgresNixosConf config else {};
      in
      lib.recursiveUpdate basic pg;

    pidfile = mkDefault "${config.dir}/vm.pid";

    image = mkDefault "${config.dir}/vm.qcow2";

    vm = mkDefault (
      let nixosArgs = { system = "x86_64-linux"; configuration = _: config.conf; };
      in (import "${global.inputs.nixpkgs}/nixos" nixosArgs).vm
    );
  };
}
