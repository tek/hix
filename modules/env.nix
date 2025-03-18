{global, util, ...}:
{name, config, lib, ...}:
# TODO remove
with lib;
let
  inherit (util) internal;

  envConfig = config;

  serviceModule = import ./service.nix { inherit lib global util; };

  envServiceModule = import ./env-service.nix { inherit lib global; };

  ghcModule = import ./ghc.nix { inherit global util; };

  vmLib = import ../lib/vm.nix { inherit (global) pkgs; inherit util; };

  exposeModule = import ./expose.nix { inherit util; type = "env"; default = false; };

  envExposeModule = import ./env-expose.nix { inherit util; };

  mkBuildInputs = def:
    if isFunction def
    then def config.ghc.pkgs
    else def;

  buildInputs =
    mkBuildInputs config.buildInputs ++
    mkBuildInputs global.buildInputs ++
    config.haskellTools config.ghc.vanillaGhc ++
    global.haskellTools config.ghc.vanillaGhc ++
    optional config.hls.enable config.hls.package ++
    optional config.ghcid.enable config.ghcid.package ++
    [config.ghcWithPackages]
    ;

  exportShellVars = vars: let
    defs = toShellVars config.env;
    exports = util.unlinesMap (n: "export ${n}") (attrNames config.env);
  in optionalString (!(util.empty vars)) ''
  ${defs}
  ${exports}
  '';

  messages = util.unlinesConcatMap (s: map (m: ''echo ">>> ${m}" >&2'') (s.messages envConfig)) resolved;

  preamble = ''
    _hix_unrestricted() {
      [[ -z ''${DIRENV_IN_ENVRC-} ]] && [[ -z ''${HIX_ONLY_ENV-} ]]
    }
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
    if _hix_unrestricted
    then
      if [[ -z ''${_hix_is_shell-} ]]
      then
        trap "quit INT" INT
      fi
      trap "quit TERM" TERM
      trap "quit KILL" KILL
      trap quit EXIT
    fi
    ${exportShellVars config.env}
    export PATH="${makeBinPath buildInputs}:$PATH"
    export env_args
    if _hix_unrestricted
    then
      :
      ${messages}
      ${config.setup-pre}
      ${optionalString config.vm.enable config.vm.setup}
      ${optionalString (config.vm.enable && config.wait > 0) (internal.env.waitScript config)}
      ${config.setup}
    fi
  '';

  runner = util.scriptErr "env-${config.name}-runner.bash" ''
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
  lib.toList service.nixos-base ++
  lib.toList service.nixos ++
  [(servicePorts service.ports)];

  resolved = filter (conf: conf.enable) (mapAttrsToList (_: s: s.resolve) config.internal.resolvedServices);

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
        description = "";
        type = str;
        default = name;
      };

      resolve = mkOption {
        description = "";
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

  ghcidMod = if util.minGhc "9.4" config then config.ghc.pkgs.haskell.lib.dontCheck else id;

in {
  options = with types; {

    enable = mkEnableOption "this environment";

    name = mkOption {
      description = "Name of this environment.";
      type = str;
      default = name;
    };

    services = mkOption {
      description = "Services for this environment.";
      type = attrsOf (submodule envServiceModule);
      default = {};
    };

    env = mkOption {
      description = "Environment variables to set when running scripts in this environment.";
      type = attrsOf (either int str);
      default = {};
    };

    packages = mkOption {
      description = ''
      The subset of local [packages](#opt-general-packages) that should be built by this environment, called its
      _targets_.

      Entries must correspond to existing keys in [](#opt-general-packages).
      If the value is `null`, all packages are included.

      This is useful when the project contains multiple sets of packages that should have separate dependency trees with
      different versions.

      Setting this has a variety of effects:
      - These packages will be exposed as outputs for this env
      - The GHC package db will not contain the excluded packages, so they won't be built when entering a shell or
        starting `.#ghci` for this env
      - Managed dependencies envs use this to produce separate sets of bounds
      '';
      type = types.nullOr (types.listOf types.str);
      default = null;
    };

    main = mkOption {
      description = ''
      The name of the main package of this env, defaulting to [](#opt-general-main) if that is in [](#opt-env-packages)
      or one of those packages determined by the same method as described
      [for the global equivalent](#opt-general-main).
      '';
      type = types.nullOr types.str;
      default = null;
    };

    ghc = mkOption {
      description = "The GHC configuration for this environment.";
      type = submodule ghcModule;
      default = {};
    };

    ghcWithPackages = mkOption {
      description = "The fully configured GHC package exposing this environment's dependencies.";
      type = package;
      readOnly = true;
    };

    ghcWithPackagesArgs = mkOption {
      description = "Additional arguments to pass to `ghcWithPackages`.";
      type = attrsOf unspecified;
      default = {};
    };

    hoogle = mkOption {
      description = "Whether to enable Hoogle in this environment.";
      type = bool;
      default = false;
    };

    overrides = mkOption {
      description = ''
      Like [](#opt-general-overrides), but used only when this environment is used to build packages.
      '';
      type = util.types.cabalOverridesVia "env ${config.name}";
      default = [];
    };

    buildInputs = mkOption {
      description = ''
      Additional system package dependencies for this environment.
      ::: {.note}
      These are only made available to shells and commands, not added to packages, like when they are set in overrides.
      :::
      '';
      type = either (functionTo (listOf package)) (listOf package);
      default = [];
    };

    haskellPackages = mkOption {
      description = ''
      Names of Haskell packages that should be added to this environment's GHC's package db, making them available for
      import.
      These may include the local packages.
      '';
      type = either (functionTo (listOf package)) (listOf str);
      default = [];
    };

    haskellTools = mkOption {
      description = ''
      Function returning a list of names of Haskell packages that should be included in the environment's `$PATH`.
      This is a convenience variant of [](#opt-env-buildInputs) that provides the environment's GHC package set (without
      overrides) as a function argument.
      This is intended for tooling like `fourmolu`.
      '';
      type = functionTo (listOf package);
      default = _: [];
      example = literalExpression ''ghc: [ghc.fourmolu]'';
    };

    localDeps = mkOption {
      description = ''
      Whether to add the dependencies of the env's local packages to GHC's package db.
      This means that those packages are visible in GHCi when run in this env, for example.
      '';
      type = bool;
      default = true;
    };

    localOverrides = mkOption {
      description = ''
      Whether to add the local packages as overrides to this env's GHC package set.

      This does not usually have any effect on outputs – it only exposes derivations on [](#opt-ghc-ghc) that can be
      ignored.
      However, if another package depends on a local package, that dep will be overridden by the local derivation.
      Imagine defining a local package named `containers` and building `cabal-install` in this env.

      If this option is set to `"targets"`, only the env's target [packages](#opt-env-packages) will be added.
      The default is to include all local packages.
      '';
      type = types.either types.bool (types.enum ["targets"]);
      default = true;
    };

    globalOverrides = mkOption {
      description = ''
      Whether to include overrides from the global option [](#opt-general-overrides).
      '';
      type = bool;
      default = true;
    };

    inheritOverrides = mkOption {
      description = ''
      Whether to include overrides from dependency flakes.
      '';
      type = bool;
      default = true;
    };

    setup-pre = mkOption {
      description = "Commands to run before the service VM has started.";
      type = str;
      default = "";
    };

    setup = mkOption {
      description = "Commands to run after the service VM has started.";
      type = str;
      default = "";
    };

    exit-pre = mkOption {
      description = "Command to run before the service VM is shut down.";
      type = str;
      default = "";
    };

    exit = mkOption {
      description = "Command to run when the env exits.";
      type = str;
      default = "";
    };

    code = mkOption {
      description = ''
      The shell script code that starts this env's services and sets its environment variables.
      '';
      type = str;
      default = preamble;
    };

    shell = mkOption {
      description = ''
      The shell derivation for this environment, starting the service VM in the `shellHook`.

      ::: {.note}
      If this shell is used with `nix develop -c`, the exit hook will never be called and the VM will not be shut down.
      Use a command instead for this purpose.
      :::
      '';
      type = package;
    };

    runner = mkOption {
      description = ''
      An executable script file that sets up the environment and executes its command line arguments.
      '';
      type = path;
      default = runner;
    };

    basePort = mkOption {
      description = "The number used as a base for ports in this env's VM, like ssh getting `basePort + 22`.";
      type = port;
      default = 20000;
    };

    defaults = mkOption {
      description = "Whether to use the common NixOS options for VMs.";
      type = bool;
      default = true;
    };

    wait = mkOption {
      description =
        "Wait for the VM to complete startup within the given number of seconds. 0 disables the feature.";
      type = int;
      default = 30;
    };

    ghcid = {
      enable = mkEnableOption "GHCid for this env" // { default = true; };

      package = mkOption {
        description = "The package for GHCid, defaulting to the one from the env's GHC without overrides.";
        type = package;
        default = ghcidMod config.ghc.vanillaGhc.ghcid;
      };
    };

    hls = {
      enable = mkEnableOption "HLS for this env";

      package = mkOption {
        description = "The package for HLS, defaulting to the one from the env's GHC without overrides.";
        type = package;
        default = config.ghc.vanillaGhc.haskell-language-server;
      };
    };

    expose = mkOption {
      description = ''
      The parts of this environment that should be accessible as flake outputs, like being able to run
      `nix build .#<env>.<package>`.
      If the value is boolean, all parts are affected.
      If it is a set, submodule options configure the individual parts.

      See [](#options-env-expose) for individual attributes.
      '';
      type = types.either types.bool (types.submoduleWith { modules = [exposeModule envExposeModule]; });
      default = false;
    };

    localPackage = mkOption {
      description = ''
      A function returning a single [override combinator expression](#opt-general-overrides) that is applied to all
      local packages in this environment.
      '';
      example = literalExpression ''
        { fast, nobench, ... }: pkg: nobench (fast pkg);
      '';
      type = functionTo unspecified;
      default = {id, ...}: id;
    };

    libraryProfiling = mkOption {
      type = bool;
      default = true;
      description = ''
        Whether to build local libraries with profiling enabled.
        This is the default mode for Haskell derivations.
      '';
    };

    profiling = mkOption {
      type = bool;
      default = false;
      description = ''
        Whether to build local libraries and executables with profiling enabled.
      '';
    };

    ifd = mkOption {
      type = bool;
      default = global.ifd;
      description = ''
      Whether to use cabal2nix, which uses Import From Derivation, or to generate simple derivations.
      '';
    };

    hostPorts = mkOption {
      description = ''
      The effective ports of the VM services in the host system.
      Computed from [](#opt-env-basePort) and [](#opt-service-ports).
      '';
      type = attrsOf port;
      readOnly = true;
    };

    systems = mkOption {
      description = ''
      The architecture/system identifiers like `x86_64-linux` for which this environment works.
      This is used to exclude environments from being exposed as shells when they are system-specific, for example when
      using a VM that only works with Linux.
      If those shells were exposed, the command `nix flake check` would fail while evaluating the `devShells` outputs,
      since that doesn't only select the current system.

      If set to `null` (the default), all systems are accepted.
      '';
      type = nullOr (listOf str);
      default = null;
    };

    managed = mkOption {
      description = ''
      Whether this env's dependencies are [](#managed).
      This has the effect that its bounds and overrides are read from the managed state in
      [](#opt-managed-managed.file).
      '';
      type = bool;
      default = false;
    };

    vm = {

      enable = mkEnableOption "the service VM for this env";

      name = mkOption {
        description = "Name of the VM, used in the directory housing the image file.";
        type = str;
        default = config.name;
      };

      dir = mkOption {
        description = "";
        type = str;
        default = ''/tmp/hix-vm/${global.name}/${config.vm.name}'';
      };

      pidfile = mkOption {
        type = str;
        description = "The file storing the qemu process' process ID.";
        default = "${config.vm.dir}/vm.pid";
      };

      image = mkOption {
        type = str;
        description = "The path to the image file.";
        default = "${config.vm.dir}/vm.qcow2";
      };

      monitor = mkOption {
        description = "The monitor socket for the VM.";
        type = str;
        default = "${config.vm.dir}/monitor";
      };

      system = mkOption {
        description = "The system architecture string used for this VM, defaulting to [](#opt-general-system).";
        type = str;
        default = global.system;
      };

      headless = mkOption {
        description = ''
        VMs are run without a graphical connection to their console.
        For debugging purposes, this option can be disabled to show the window.
        '';
        type = bool;
        default = true;
      };

      setup = mkOption {
        description = "Commands for starting the VM.";
        type = str;
      };

      exit = mkOption {
        description = "Commands for shutting down the VM.";
        type = str;
      };

      derivation = mkOption {
        description = "The VM derivation";
        type = path;
      };

    };

    internal = {

      overridesLocal = mkOption {
        description = "The local packages, encoded as overrides.";
        type = util.types.cabalOverridesVia "project";
      };

      overridesEnvUnmanaged = mkOption {
        description = "Overrides for this env, excluding extras like managed dependencies.";
        type = util.types.cabalOverridesVia "computed env ${config.name} unmanaged";
        readOnly = true;
      };

      overridesEnv = mkOption {
        description = "Overrides for this env, including extras like managed dependencies.";
        type = util.types.cabalOverridesVia "computed env ${config.name}";
        readOnly = true;
      };

      overridesInherited = mkOption {
        description = "The inherited overrides used for this env.";
        type = util.types.cabalOverridesVia "inherited by env ${config.name}";
        default = [];
      };

      overridesSolver = mkOption {
        description = "Overrides for this env for use with the solver in managed dependency apps.";
        type = util.types.cabalOverridesVia "computed env ${config.name} solver";
        default = [];
      };

      resolvedServices = mkOption {
        description = "Magic modules that allow merging env-specific service config with their base config.";
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

    ghc = let

      inherited = lib.optionals config.inheritOverrides;

      overrideSources =
        if config.managed
        then [
          (inherited (util.overridesFromDeps ["local"]))
          config.internal.overridesLocal
          config.internal.overridesEnv
        ]
        else [
          (inherited config.internal.overridesInherited)
          config.internal.overridesLocal
          (lib.optionals config.globalOverrides global.overrides)
          config.internal.overridesEnv
        ];

    in {
      name = mkDefault config.name;

      overrides = mkDefault (util.concatOverrides overrideSources);

      gen-overrides = mkDefault true;
    };

    hostPorts = util.mapListCatAttrs (s: mapAttrs (_: effectiveHostPort) s.ports) resolved;

    shell = mkDefault (global.pkgs.stdenv.mkDerivation {
      inherit (config) name;
      inherit buildInputs;
      shellHook = ''
      _hix_is_shell=1
      ${config.code}
      '';
    });

    ghcWithPackages =
      util.ghc.packageDbFull config ({ withHoogle = config.hoogle; } // config.ghcWithPackagesArgs);

    vm = {

      enable = let
        builtInCount = (if config.wait > 0 then 1 else 0) + (if config.defaults then 1 else 0);
      in mkDefault (length resolved > builtInCount);

      derivation = mkDefault (
        let nixosArgs = {
          inherit (config.vm) system;
          modules = combinedConfig;
        };
        in (lib.nixosSystem nixosArgs).config.system.build.vm
        );

        setup = mkDefault "${vmLib.ensure config.basePort config.vm}";
        exit = mkDefault "${vmLib.kill config.vm}";

      };

      internal = {

        overridesLocal = import ../lib/deps/local.nix {
          inherit util;
          inherit (config) ifd localPackage libraryProfiling profiling;
          env = config.name;
          packages =
            if config.localOverrides == "targets"
            then util.restrictKeys (internal.env.targets config) global.packages
            else if config.localOverrides
            then global.packages
            else {};
        };

        overridesEnvUnmanaged = lib.toList config.overrides;

        overridesEnv = util.concatOverrides (
          optional config.managed (internal.env.managedOverrides config.name)
          ++
          [config.overrides]
        );

        overridesInherited = util.unlessDev config global.envs.dev.internal.overridesInherited;

      };

  };
}
