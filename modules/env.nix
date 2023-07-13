{global, util, ...}:
{name, config, lib, ...}:
with lib;
let

  envConfig = config;

  serviceModule = import ./service.nix { inherit lib global; };

  envServiceModule = import ./env-service.nix { inherit lib global; };

  ghcModule = import ./ghc.nix { inherit global util; };

  vmLib = import ../lib/vm.nix { inherit (global) pkgs; };

  waitSeconds = toString config.wait;

  waitScript = ''
  running=0
  echo ">>> Waiting ${waitSeconds} seconds for VM to boot..." >&2
  timeout=$(( $SECONDS + ${waitSeconds} ))
  while (( $SECONDS < $timeout ))
  do
    pong=$(${global.pkgs.socat}/bin/socat -T 1 - TCP:localhost:${toString (config.hostPorts.hix-internal-env-wait)} <<< 'ping' 2>&1)
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

  customBuildInputs =
    if isFunction config.buildInputs
    then config.buildInputs config.ghc.pkgs
    else config.buildInputs;

  extraHs = ghc:
  if isFunction config.haskellPackages
  then config.haskellPackages ghc
  else map (n: ghc.${n}) config.haskellPackages;

  ghcWithPackages = let
    bInputs = p: p.buildInputs ++ p.propagatedBuildInputs;
    isNotLocal = p: !(p ? pname && elem p.pname global.internal.packageNames);
    localDeps = g: builtins.filter isNotLocal (concatMap bInputs (map (p: g.${p}) global.internal.packageNames));
  in config.ghc.ghc.ghcWithPackages (ghc: optionals config.localDeps (localDeps ghc) ++ extraHs ghc ++ [ghc.cabal-install]);

  buildInputs = let
  in
  customBuildInputs ++
  optional config.hls.enable config.hls.package ++
  optional config.ghcid.enable config.ghcid.package ++
  [ghcWithPackages]
  ;

  exportShellVars = vars: let
    defs = toShellVars config.env;
    exports = concatMapStringsSep "\n" (n: "export ${n}") (attrNames config.env);
  in optionalString (!(util.empty vars)) ''
  ${defs}
  ${exports}
  '';

  messages = util.unlinesConcatMap (s: map (m: ''echo ">>> ${m}" >&2'') (s.messages envConfig)) resolved;

  preamble = ''
    quitting=0
    quit() {
      if [[ $quitting == 0 ]]
      then
        quitting=1
        if [[ -n ''${1-} ]]
        then
          echo ">>> Terminated by signal $1" >&2
        fi
        ${config.exit-pre}
        ${optionalString config.vm.enable config.vm.exit}
        ${config.exit}
        # kill zombie GHCs
        ${global.pkgs.procps}/bin/pkill -9 -x -P 1 ghc || true
      fi
      if [[ -n ''${1-} ]]
      then
        exit 1
      fi
    }
    if [[ -z ''${_hix_is_shell-} ]]
    then
      trap "quit INT" INT
    fi
    trap "quit TERM" TERM
    trap "quit KILL" KILL
    trap quit EXIT
    ${exportShellVars config.env}
    export PATH="${makeBinPath buildInputs}:$PATH"
    export env_args
    ${messages}
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

  effectiveHostPort = { host, absolute ? false, ... }:
  if absolute then host else config.basePort + host;

  servicePort = p:
  { host.port = effectiveHostPort p; guest.port = p.guest; };

  servicePorts = ports: {
    virtualisation.vmVariant.virtualisation.forwardPorts = map servicePort (attrValues ports);
  };

  vmConfig = {
    virtualisation.vmVariant.virtualisation = {
      diskImage = config.vm.image;
      diskSize = 4096;
    };
  };

  nixosDefaults = {
    networking.firewall.enable = false;
    documentation.nixos.enable = false;
    system.stateVersion = "22.05";
  };

  serviceConfig = service:
  [service.nixos-base service.nixos (servicePorts service.ports)];

  resolved =
  mapAttrsToList (_: s: s.resolve) config.internal.resolvedServices;

  combinedConfig =
    [vmConfig] ++
    optional config.defaults nixosDefaults ++
    concatMap serviceConfig resolved
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
          modules = optionals (name != "‹name›") ([
            serviceModule
            { inherit (service) enable; }
          ] ++
          optional (hasAttr name global.services) global.services.${name} ++
          optionals (hasAttr name global.internal.services) [
            global.internal.services.${name}
            service.config
          ]);
        };
        default = {};
      };
    };
  };

  ghc = config.ghc.ghc;

  extraPackages = genAttrs global.output.extraPackages (n: ghc.${n});

  localPackages = genAttrs global.internal.packageNames (n: ghc.${n} // { inherit ghc; });

  ghcidMod = if util.minGhc "9.4" config then config.ghc.pkgs.haskell.lib.dontCheck else id;

in {
  options = with types; {

    enable = mkEnableOption (mdDoc "this environment");

    name = mkOption {
      description = mdDoc "Name of this environment.";
      type = str;
      default = name;
    };

    services = mkOption {
      description = mdDoc "Services for this environment.";
      type = attrsOf (submodule envServiceModule);
      default = {};
    };

    env = mkOption {
      description = mdDoc "Environment variables to set when running scripts in this environment.";
      type = attrsOf (either int str);
      default = {};
    };

    ghc = mkOption {
      description = mdDoc "The GHC configuration for this environment.";
      type = submodule ghcModule;
      default = {};
    };

    ghcWithPackages = mkOption {
      description = mdDoc "The fully configured GHC package exposing this environment's dependencies.";
      type = package;
      readOnly = true;
      default = ghcWithPackages;
    };

    overrides = mkOption {
      description = mdDoc ''
      Like [](#opt-general-overrides), but used only when this environment is used to build packages.
      '';
      type = util.types.cabalOverrides;
      default = [];
    };

    buildInputs = mkOption {
      description = mdDoc "Additional system package dependencies for this environment.";
      type = either (functionTo (listOf package)) (listOf package);
      default = [];
    };

    haskellPackages = mkOption {
      description = mdDoc ''
      Names of Haskell packages that should be added to this environment's GHC's package db, making them available for
      import.
      These may include the local packages.
      '';
      type = either (functionTo (listOf package)) (listOf str);
      default = [];
    };

    localDeps = mkOption {
      description = mdDoc "Whether to add the dependencies of the project's local packages to GHC's package db.";
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
      description = mdDoc ''
      The shell script code that starts this env's services and sets its environment variables.
      '';
      type = str;
      default = preamble;
    };

    shell = mkOption {
      description = mdDoc ''
      The shell derivation for this environment, starting the service VM in the `shellHook`.

      ::: {.note}
      If this shell is used with `nix develop -c`, the exit hook will never be called and the VM will not be shut down.
      Use a command instead for this purpose.
      :::
      '';
      type = package;
    };

    runner = mkOption {
      description = mdDoc ''
      An executable script file that sets up the environment and executes its command line arguments.
      '';
      type = path;
      default = runner;
    };

    basePort = mkOption {
      description = mdDoc "The number used as a base for ports in this env's VM, like ssh getting `basePort + 22`.";
      type = port;
      default = 20000;
    };

    defaults = mkOption {
      description = mdDoc "Whether to use the common NixOS options for VMs.";
      type = bool;
      default = true;
    };

    wait = mkOption {
      description =
        mdDoc "Wait for the VM to complete startup within the given number of seconds. 0 disables the feature.";
      type = int;
      default = 30;
    };

    ghcid = {
      enable = mkEnableOption (mdDoc "GHCid for this env") // { default = true; };

      package = mkOption {
        description = mdDoc "The package for GHCid, defaulting to the one from the env's GHC without overrides.";
        type = package;
        default = ghcidMod config.ghc.vanillaGhc.ghcid;
      };
    };

    hls = {
      enable = mkEnableOption (mdDoc "HLS for this env");

      package = mkOption {
        description = mdDoc "The package for HLS, defaulting to the one from the env's GHC without overrides.";
        type = package;
        default = config.ghc.vanillaGhc.haskell-language-server;
      };
    };

    hide = mkOption {
      description = mdDoc "Skip this env for user-facing actions, like command exposition in `apps`.";
      type = bool;
      default = false;
    };

    derivations = mkOption {
      description = mdDoc ''
      The derivations for the local Cabal packages using this env's GHC, as well as the
      [](#opt-general-output.extraPackages).
      '';
      type = lazyAttrsOf package;
      default = localPackages // extraPackages;
    };

    localPackage = mkOption {
      description = mdDoc ''
        A function that takes dep combinators and a derivation and returns a modified version of that derivation.
        Called for each cabal2nix derivation of the local packages before inserting it into the overrides.

        The default is to disable profiling if [](#opt-env-profiling) is `false`.
        If this option is customized, the profiling option won't be effective.
      '';
      example = literalExpression ''
        { fast, nobench, ... }: pkg: nobench (fast pkg);
      '';
      type = unspecified;
      default = api: if config.profiling then id else api.noprofiling;
    };

    profiling = mkOption {
      type = bool;
      default = true;
      description = mdDoc ''
        Whether to build local packages and dependency overrides with profiling enabled.
        Ineffective if [](#opt-env-localPackage) is customized.
      '';
    };

    ifd = mkOption {
      type = bool;
      default = global.ifd;
      description = mdDoc ''
      Whether to use cabal2nix, which uses Import From Derivation, or to generate simple derivations.
      '';
    };

    auto = mkOption {
      type = bool;
      default = global.auto;
      description = mdDoc ''
      Generate the Cabal file on the fly if none is present in the source directory (or a `package.yaml`).
      '';
    };

    hostPorts = mkOption {
      description = mdDoc ''
      The effective ports of the VM services in the host system.
      Computed from [](#opt-env-basePort) and [](#opt-service-ports).
      '';
      type = attrsOf port;
      readOnly = true;
    };

    vm = {

      enable = mkEnableOption (mdDoc "the service VM for this env");

      name = mkOption {
        description = mdDoc "Name of the VM, used in the directory housing the image file.";
        type = str;
        default = config.name;
      };

      dir = mkOption {
        description = mdDoc "";
        type = str;
        default = "/tmp/hix-vm/$USER/${config.vm.name}";
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

      monitor = mkOption {
        description = mdDoc "The monitor socket for the VM.";
        type = str;
        default = "${config.vm.dir}/monitor";
      };

      system = mkOption {
        description = mdDoc "The system architecture string used for this VM, defaulting to [](#opt-general-system).";
        type = str;
        default = global.system;
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

      overridesLocal = mkOption {
        description = mdDocs "The local packages, encoded as overrides.";
        type = util.types.cabalOverrides;
        default = import ../lib/deps/local.nix {
          config = global;
          inherit lib;
          inherit (config) ifd auto localPackage;
        };
      };

      overridesInherited = mkOption {
        description = mdDoc "The inherited overrides used for this env.";
        type = util.types.cabalOverrides;
        default = [];
      };

      resolvedServices = mkOption {
        description = mdDoc "Magic modules that allow merging of env-specific service config with their base config.";
        type = attrsOf (submodule resolveServiceModule);
        default = mapAttrs (_: _: {}) config.services;
        readOnly = true;
      };

    };

  };

  config = {

    enable = mkDefault true;

    services.hix-internal-env-wait.enable = config.wait > 0;

    services.ssh.enable = config.defaults;

    ghc.overrides = mkDefault (
      util.concatOverrides [config.internal.overridesLocal config.internal.overridesInherited config.overrides]
    );

    hostPorts = util.foldMapAttrs (s: mapAttrs (_: effectiveHostPort) s.ports) resolved;

    shell = mkDefault (global.pkgs.stdenv.mkDerivation {
      inherit (config) name;
      inherit buildInputs;
      shellHook = ''
      _hix_is_shell=1
      ${config.code}
      '';
    });

    vm = {

      enable = let
        builtInCount = (if config.wait > 0 then 1 else 0) + (if config.defaults then 1 else 0);
      in mkDefault (length (attrNames config.services) > builtInCount);

      derivation = mkDefault (
        let nixosArgs = {
          inherit (config.vm) system;
          modules = combinedConfig;
        };
        in (lib.nixosSystem nixosArgs).config.system.build.vm
        );

        setup = mkDefault "${vmLib.ensure config.vm}";
        exit = mkDefault "${vmLib.kill config.vm}";

      };

    internal.overridesInherited = util.unlessDev config global.envs.dev.internal.overridesInherited;

  };
}
