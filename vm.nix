{ nixpkgs, pkgs, ... }:
with pkgs.lib;
let
  merge = recursiveUpdateUntil (_: lhs: rhs: !(isAttrs lhs && isAttrs rhs) || isDerivation lhs || isDerivation rhs);

  create = {
    name ? "hix-vm",
    dir ? "/tmp/hix-vm/$USER/${name}",
    basePort ? 10000,
    ports ? [],
    conf ? {},
    ...
  }:
  let
    pidfile = "${dir}/vm.pid";

    image = "${dir}/vm.qcow2";

    nixos = import "${nixpkgs}/nixos" {
      system = "x86_64-linux";
      configuration = { pkgs, ... }:
      let
        basic = {
          virtualisation = {
            diskImage = image;
            diskSize = 4096;
            forwardPorts = [
              { from = "host"; host.port = basePort + 22; guest.port = 22; }
            ] ++ ports;
          };
          services.openssh = {
            enable = true;
            permitRootLogin = "yes";
          };
          users.mutableUsers = false;
          users.users.root.password = "";
          networking.firewall.enable = false;
        };
        custom = if isAttrs conf then conf else conf pkgs;
      in merge basic custom;
    };
  in {
    main = nixos.vm;
    inherit dir pidfile image pkgs;
  };

  postgres = {
    name,
    dbName ? name,
    port ? 10000,
    dir ? "/tmp/hix-vm/$USER/${name}",
    ports ? [],
    creds ? {},
    log ? false,
    conf ? {},
    ...
  }:
  let
    user = creds.user or name;
  in create {
    inherit name dir;
    basePort = port;
    ports = ports ++ [{ from = "host"; host.port = port; guest.port = 5432; }];
    conf = pkgs: {
      services.postgresql = {
        enable = true;
        package = pkgs.postgresql_13;
        enableTCPIP = true;
        ensureDatabases = [dbName];
        authentication = ''
          host all all 0.0.0.0/0 md5
          host all all ::/0 md5
        '';
        initialScript = pkgs.writeText "vm-postgresql-init" ''
          create role "${user}" with login password '${creds.password or dbName}' createdb;
          grant all privileges on database "${dbName}" to "${user}";
        '';
        settings = if log then { log_statement = "all"; log_min_messages = "info"; } else {};
      };
    } // (if isAttrs conf then conf else conf pkgs);
  };

  ensure = vm: pkgs.writeScript "ensure-vm" ''
    #!${pkgs.zsh}/bin/zsh
    if ${pkgs.procps}/bin/pgrep -F ${vm.pidfile} -L -f ${vm.pidfile} &>/dev/null
    then
      print '>>> vm already running' >&2
    else
      print '>>> starting vm' >&2
      mkdir -p ${vm.dir}
      rm -f ${vm.pidfile}
      ${vm.main}/bin/run-nixos-vm -display none -daemonize -pidfile ${vm.pidfile}
    fi
  '';

  kill = vm: pkgs.writeScript "kill-vm" ''
    #!${pkgs.zsh}/bin/zsh
    pid=$(${pkgs.procps}/bin/pgrep -F ${vm.pidfile} -L -f ${vm.pidfile})
    if [[ $? == 0 ]]
    then
      print '>>> killing vm' >&2
      kill $pid
    else
      print '>>> vm not running' >&2
    fi
  '';

in {
  inherit create postgres ensure kill;
}
