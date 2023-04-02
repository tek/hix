{global, util, ...}:
{name, config, lib, ...}:
with lib;
let

  envConfig = config;

  serviceModule = import ./service.nix { inherit lib global; };

  envServiceModule = import ./env-service.nix { inherit lib global; };

  ghcModule = import ./ghc.nix { inherit global util; };

  vmLib = import ../lib/vm-new.nix { inherit (global) pkgs; };

  waitSeconds = toString config.wait;

  waitScript = ''
  running=0
  echo ">>> Waiting ${waitSeconds} seconds for VM to boot..." >&2
  timeout=$(( $SECONDS + ${waitSeconds} ))
  while (( $SECONDS < $timeout ))
  do
    pong=$(${global.pkgs.socat}/bin/socat -T 1 - TCP:localhost:${toString (config.basePort + 1)} <<< 'ping' 2>&1)
    if [[ $pong == 'running' ]]
    then
      running=1
      break
    else
      sleep 0.1
    fi
  done
  if [[ $running == 0 ]]
  then
    echo ">>> VM wasn't ready after ${waitSeconds} seconds." >&2
    exit 1
  fi
  '';

  # TODO move to env config
  vanillaGhc = global.envs.dev.ghc.vanillaGhc;
  wantGhcid = global.shell.ghcid.enable;
  vanillaGhcid = global.shell.ghcid.vanilla;

  ghcidDep =
  if vanillaGhcid
  then global.pkgs.haskell.lib.dontCheck vanillaGhc.ghcid
  else global.envs.dev.ghc.ghc.ghcid;

  buildInputs = let
    isNotLocal = p: !(p ? pname && elem p.pname global.internal.packageNames);
    bInputs = p: p.buildInputs ++ p.propagatedBuildInputs;
    localDeps = g: builtins.filter isNotLocal (concatMap bInputs (map (p: g.${p}) global.internal.packageNames));
  in
  config.buildInputs ++
  [global.shell.hls.package] ++
  optional wantGhcid ghcidDep ++
  [(config.ghc.ghc.ghcWithPackages (ghc: optionals config.localDeps (localDeps ghc) ++ map (n: ghc.${n}) config.haskellPackages))]
  ;

  exportShellVars = vars:
  optionalString (!(util.empty vars)) "export ${toShellVars config.env}";

  preamble = ''
    quitting=0
    quit() {
      if [[ $quitting == 0 ]]
      then
        quitting=1
        if [[ -n $1 ]]
        then
          echo ">>> terminated by signal $1" >&2
        fi
        ${config.exit-pre}
        ${optionalString config.vm.enable config.vm.exit}
        ${config.exit}
        # kill zombie GHCs
        ${global.pkgs.procps}/bin/pkill -9 -x -P 1 ghc
      fi
      if [[ -n $1 ]]
      then
        exit 1
      fi
    }
    trap "quit INT" INT
    trap "quit TERM" TERM
    trap "quit KILL" KILL
    trap quit EXIT
    ${exportShellVars config.env}
    export PATH="${makeBinPath buildInputs}:$PATH"
    ${config.setup-pre}
    ${optionalString config.vm.enable config.vm.setup}
    ${optionalString (config.vm.enable && config.wait > 0) waitScript}
    ${config.setup}
  '';

  runner = global.pkgs.writeScript "env-${config.name}-runner.bash" ''
  #!${global.pkgs.bashInteractive}/bin/bash
   ${config.code}
   $@
  '';

  setupVm = ''
  ${vmLib.ensure config.vm}
  '';

  exitVm = ''
  ${vmLib.kill config.vm}
  '';

  servicePort = { guest, host }:
  { host.port = config.basePort + host; guest.port = guest; };

  servicePorts = ports: {
    virtualisation.vmVariant.virtualisation.forwardPorts = map servicePort ports;
  };

  vmConfig = {
    virtualisation.vmVariant.virtualisation = {
      diskImage = config.vm.image;
      diskSize = 4096;
    };
  };

  nixosDefaults = servicePorts [{ guest = 22; host = 22; }] // {
    services.openssh = {
      enable = true;
      permitRootLogin = "yes";
    };
    users.mutableUsers = true;
    users.users.root.password = "";
    networking.firewall.enable = false;
    documentation.nixos.enable = false;
    system.stateVersion = "22.05";
  };

  serviceConfig = service:
  [service.nixos-base service.nixos (servicePorts service.ports)];

  combinedConfig =
    [vmConfig] ++
    optional config.defaults nixosDefaults ++
    concatMap (s: serviceConfig s.resolve) (attrValues config.internal.resolvedServices)
    ;

  resolveServiceModule = {name, config, ...}: let
    service = envConfig.services.${name};
  in {
    options = with types; {
      name = mkOption {
        description = mdDoc "";
        type = str;
        default = name;
      };

      resolve = mkOption {
        description = mdDoc "";
        type = submoduleWith {
          modules = optionals (name != "‹name›") [
            serviceModule
            global.services.${name}
            { inherit (service) enable; }
            service.config
          ] ++
          optional (hasAttr name global.service) global.service.${name};
        };
        default = {};
      };
    };
  };

  resolveService = n: s:
  {};

in {
  options = with types; {

    enable = mkEnableOption (mdDoc "this env");

    name = mkOption {
      description = mdDoc "Env name";
      type = str;
      default = name;
    };

    services = mkOption {
      description = mdDoc "Services for this env";
      type = attrsOf (submodule envServiceModule);
      default = {};
    };

    env = mkOption {
      description = mdDoc "Environment variables";
      type = attrsOf str;
      default = {};
    };

    ghc = mkOption {
      description = mdDoc "";
      type = submodule ghcModule;
      default = {};
    };

    overrides = mkOption {
      type = util.types.cabalOverrides;
      default = [];
      description = mdDoc ''
      TODO
      '';
    };

    buildInputs = mkOption {
      description = mdDoc "";
      type = listOf package;
      default = [];
    };

    haskellPackages = mkOption {
      description = mdDoc "";
      type = listOf str;
      default = [];
    };

    localDeps = mkOption {
      description = mdDoc "Add dependencies of local packages.";
      type = bool;
      default = true;
    };

    setup-pre = mkOption {
      description = mdDoc "Commands to run before the service VM has started.";
      type = str;
      default = "";
    };

    setup = mkOption {
      description = mdDoc "Commands to run after the service VM has started.";
      type = str;
      default = "";
    };

    exit-pre = mkOption {
      description = mdDoc "Command to run before the service VM is shut down.";
      type = str;
      default = "";
    };

    exit = mkOption {
      description = mdDoc "Command to run when the env exits.";
      type = str;
      default = "";
    };

    code = mkOption {
      description = mdDoc "";
      type = str;
      default = preamble;
    };

    runner = mkOption {
      description = mdDoc "";
      type = path;
      default = runner;
    };

    basePort = mkOption {
      description = mdDoc "The number as a base for ports in this env's VM, like ssh getting `basePort + 22`.";
      type = port;
      default = 20000;
    };

    defaults = mkOption {
      description = mdDoc "Whether to use the common NixOS options for VMs.";
      type = bool;
      default = true;
    };

    wait = mkOption {
      description = mdDoc "Wait for the VM to complete startup within the given number of seconds. 0 disables the feature.";
      type = int;
      default = 30;
    };

    ghcid = mkOption {
      description = mdDoc "";
      type = bool;
      default = true;
    };

    hls = mkOption {
      description = mdDoc "";
      type = bool;
      default = true;
    };

    vm = {

      enable = mkEnableOption (mdDoc "the service VM for this env");

      dir = mkOption {
        description = mdDoc "";
        type = str;
        default = "/tmp/hix-vm/$USER/${config.name}";
      };

      pidfile = mkOption {
        type = str;
        description = mdDoc "The file storing the qemu process' process ID.";
        default = "${config.vm.dir}/vm.pid";
      };

      image = mkOption {
        type = str;
        description = mdDoc "The path to the image file.";
        default = "${config.vm.dir}/vm.qcow2";
      };

      headless = mkOption {
        description = mdDoc ''
        VMs are run without a graphical connection to their console.
        For debugging purposes, this option can be disabled to show the window.
        '';
        type = bool;
        default = true;
      };

      setup = mkOption {
        description = mdDoc "Commands for starting the VM.";
        type = str;
      };

      exit = mkOption {
        description = mdDoc "Commands for shutting down the VM.";
        type = str;
      };

      derivation = mkOption {
        description = mdDoc "The VM derivation";
        type = path;
      };

    };

    internal = {

      overridesInherited = mkOption {
        type = util.types.cabalOverrides;
        description = mdDoc "The inherited overrides used for this env, like local packages and global overrides.";
        default = global.internal.overridesLocal;
      };

      resolvedServices = mkOption {
        description = mdDoc "";
        type = attrsOf (submodule resolveServiceModule);
        default = mapAttrs resolveService config.services;
      };

    };

  };

  config = {

    enable = mkDefault true;

    services.hix-internal-env-wait = {
      enable = config.wait > 0;
      config = { inherit (config) wait; };
    };

    ghc.overrides = mkDefault (util.concatOverrides [config.internal.overridesInherited config.overrides]);

    vm = {

      enable = mkDefault (length (attrNames config.services) > (if config.wait > 0 then 1 else 0));

      derivation = mkDefault (
        let nixosArgs = {
          system = "x86_64-linux";
          modules = combinedConfig;
        };
        in (lib.nixosSystem nixosArgs).config.system.build.vm
        );

        setup = mkDefault setupVm;
        exit = mkDefault exitVm;

      };

  };
}
