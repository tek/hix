{global, util, ...}:
{name, config, lib, ...}:
let
  inherit (util) internal build;
  inherit (lib) types;

  built = build.envs.${name};

  envServiceModule = import ./env/service.nix { inherit lib global; };

  ghcModule = import ./ghc.nix { inherit util; };

  exposeModule = import ./expose.nix { inherit util; type = "env"; default = false; };

  envExposeModule = import ./env/expose.nix { inherit util; };

  resolveServiceModule = import ./env/resolve-service.nix { inherit util; env = config; };

  runner = util.scriptErr "env-${config.name}-runner.bash" ''
   ${config.code}
   "$@"
  '';

  effectiveHostPort = { host, absolute ? false, ... }:
  if absolute then host else config.basePort + host;

  servicePort = p:
  { host.port = effectiveHostPort p; guest.port = p.guest; };

  servicePorts = ports: {
    virtualisation.vmVariant.virtualisation.forwardPorts = map servicePort (lib.attrValues ports);
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

  combinedConfig =
    [vmConfig] ++
    lib.optional config.defaults nixosDefaults ++
    lib.concatMap serviceConfig built.resolvedServices
    ;

  ghcidMod = if util.minGhc "9.4" config then built.toolchain.pkgs.haskell.lib.dontCheck else lib.id;

in {
  options = let

    inherit (types) str bool either functionTo listOf package;

  in {

    enable = lib.mkEnableOption "this environment";

    name = lib.mkOption {
      description = "Name of this environment.";
      type = str;
      default = name;
    };

    services = lib.mkOption {
      description = "Services for this environment.";
      type = types.attrsOf (types.submodule envServiceModule);
      default = {};
    };

    env = lib.mkOption {
      description = "Environment variables to set when running scripts in this environment.";
      type = types.attrsOf (either types.int str);
      default = {};
    };

    packages = lib.mkOption {
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

    main = lib.mkOption {
      description = ''
      The name of the main package of this env, defaulting to [](#opt-general-main) if that is in [](#opt-env-packages)
      or one of those packages determined by the same method as described
      [for the global equivalent](#opt-general-main).
      '';
      type = types.nullOr types.str;
      default = null;
    };

    compiler = util.maybeOption types.str {
      description = ''
      The name of a compiler configuration in [](#opt-general-main) or an attribute in nixpkgs' `haskell.packages`, like
      `"ghc910"`.
      This is a convenience option for configuring [the compiler option](#opt-package-set-compiler) in the env's
      [package set](#opt-env-package-set).
      '';
    };

    package-set = internal.modules.extensibleOption {
      module = util.types.package-set;
      type = util.types.ref.package-set;
      attr = "package-sets";
      desc = "that provides the packages for this environment";
      extender = config.name;
    };

    toolchain = lib.mkOption {
      description = ''
      The pkgs, compiler and package set configured by [](#opt-env-package-set).
      '';
      type = util.types.toolchain;
    };

    ghc = lib.mkOption {
      description = "Deprecation shim.";
      type = types.submodule ghcModule;
    };

    ghcWithPackages = lib.mkOption {
      description = "The fully configured GHC package exposing this environment's dependencies.";
      type = types.package;
      readOnly = true;
    };

    ghcWithPackagesArgs = lib.mkOption {
      description = "Additional arguments to pass to `ghcWithPackages`.";
      type = types.attrsOf types.unspecified;
      default = {};
    };

    hoogle = lib.mkOption {
      description = "Whether to enable Hoogle in this environment.";
      type = types.bool;
      default = false;
    };

    overrides = lib.mkOption {
      description = ''
      Like [](#opt-general-overrides), but used only when this environment is used to build packages.
      '';
      type = util.types.cabalOverridesVia "env ${config.name}";
      default = [];
    };

    buildInputs = lib.mkOption {
      description = ''
      Additional system package dependencies for this environment.
      ::: {.note}
      These are only made available to shells and commands, not added to packages, like when they are set in overrides.
      :::
      '';
      type = either (functionTo (listOf package)) (listOf package);
      default = [];
    };

    haskellPackages = lib.mkOption {
      description = ''
      Names of Haskell packages that should be added to this environment's GHC's package db, making them available for
      import.
      These may include the local packages.
      '';
      type = either (functionTo (listOf package)) (listOf str);
      default = [];
    };

    haskellTools = lib.mkOption {
      description = ''
      Function returning a list of names of Haskell packages that should be included in the environment's `$PATH`.
      This is a convenience variant of [](#opt-env-buildInputs) that provides the environment's GHC package set (without
      overrides) as a function argument.
      This is intended for tooling like `fourmolu`.
      '';
      type = functionTo (listOf package);
      default = _: [];
      example = lib.literalExpression ''ghc: [ghc.fourmolu]'';
    };

    localDeps = lib.mkOption {
      description = ''
      Whether to add the dependencies of the env's local packages to GHC's package db.
      This means that those packages are visible in GHCi when run in this env, for example.
      '';
      type = bool;
      default = true;
    };

    localOverrides = lib.mkOption {
      description = ''
      Whether to add the local packages as overrides to this env's GHC package set.

      This does not usually have any effect on outputs – it only exposes derivations on [](#opt-env-toolchain.packages)
      that can be ignored.
      However, if another package depends on a local package, that dep will be overridden by the local derivation.
      Imagine defining a local package named `containers` and building `cabal-install` in this env.

      If this option is set to `"targets"`, only the env's target [packages](#opt-env-packages) will be added.
      The default is to include all local packages.
      '';
      type = types.either types.bool (types.enum ["targets"]);
      default = true;
    };

    globalOverrides = lib.mkOption {
      description = ''
      Whether to include overrides from the global option [](#opt-general-overrides).
      '';
      type = bool;
      default = true;
    };

    inheritOverrides = lib.mkOption {
      description = ''
      Whether to include overrides from dependency flakes.
      '';
      type = bool;
      default = true;
    };

    setup-pre = lib.mkOption {
      description = "Commands to run before the service VM has started.";
      type = str;
      default = "";
    };

    setup = lib.mkOption {
      description = "Commands to run after the service VM has started.";
      type = str;
      default = "";
    };

    exit-pre = lib.mkOption {
      description = "Command to run before the service VM is shut down.";
      type = str;
      default = "";
    };

    exit = lib.mkOption {
      description = "Command to run when the env exits.";
      type = str;
      default = "";
    };

    code = lib.mkOption {
      description = ''
      The shell script code that starts this env's services and sets its environment variables.
      '';
      type = str;
    };

    shell = lib.mkOption {
      description = ''
      The shell derivation for this environment, starting the service VM in the `shellHook`.

      ::: {.note}
      If this shell is used with `nix develop -c`, the exit hook will never be called and the VM will not be shut down.
      Use a command instead for this purpose.
      :::
      '';
      type = package;
    };

    runner = lib.mkOption {
      description = ''
      An executable script file that sets up the environment and executes its command line arguments.
      '';
      type = types.path;
      default = runner;
    };

    basePort = lib.mkOption {
      description = "The number used as a base for ports in this env's VM, like ssh getting `basePort + 22`.";
      type = types.port;
      default = 20000;
    };

    defaults = lib.mkOption {
      description = "Whether to use the common NixOS options for VMs.";
      type = bool;
      default = true;
    };

    wait = lib.mkOption {
      description =
        "Wait for the VM to complete startup within the given number of seconds. 0 disables the feature.";
      type = types.int;
      default = 30;
    };

    ghcid = {
      enable = lib.mkEnableOption "GHCid for this env" // { default = true; };

      package = lib.mkOption {
        description = "The package for GHCid, defaulting to the one from the env's GHC without overrides.";
        type = package;
      };
    };

    hls = {
      enable = lib.mkEnableOption "HLS for this env";

      package = lib.mkOption {
        description = "The package for HLS, defaulting to the one from the env's GHC without overrides.";
        type = package;
      };
    };

    expose = lib.mkOption {
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

    localPackage = lib.mkOption {
      description = ''
      A function returning a single [override combinator expression](#opt-general-overrides) that is applied to all
      local packages in this environment.
      '';
      example = lib.literalExpression ''
        { fast, nobench, ... }: pkg: nobench (fast pkg);
      '';
      type = functionTo types.unspecified;
      default = {id, ...}: id;
    };

    libraryProfiling = lib.mkOption {
      type = bool;
      default = true;
      description = ''
        Whether to build local libraries with profiling enabled.
        This is the default mode for Haskell derivations.
      '';
    };

    profiling = lib.mkOption {
      type = bool;
      default = false;
      description = ''
        Whether to build local libraries and executables with profiling enabled.
      '';
    };

    ifd = lib.mkOption {
      type = bool;
      default = global.ifd;
      description = ''
      Whether to use cabal2nix, which uses Import From Derivation, or to generate simple derivations.
      '';
    };

    hostPorts = lib.mkOption {
      description = ''
      The effective ports of the VM services in the host system.
      Computed from [](#opt-env-basePort) and [](#opt-service-ports).
      '';
      type = types.attrsOf types.port;
      readOnly = true;
    };

    systems = lib.mkOption {
      description = ''
      The architecture/system identifiers like `x86_64-linux` for which this environment works.
      This is used to exclude environments from being exposed as shells when they are system-specific, for example when
      using a VM that only works with Linux.
      If those shells were exposed, the command `nix flake check` would fail while evaluating the `devShells` outputs,
      since that doesn't only select the current system.

      If set to `null` (the default), all systems are accepted.
      '';
      type = types.nullOr (listOf str);
      default = null;
    };

    managed = lib.mkOption {
      description = ''
      Whether this env's dependencies are [](#managed).
      This has the effect that its bounds and overrides are read from the managed state in
      [](#opt-managed-managed.file).
      '';
      type = bool;
      default = false;
    };

    vm = {

      enable = lib.mkEnableOption "the service VM for this env";

      name = lib.mkOption {
        description = "Name of the VM, used in the directory housing the image file.";
        type = str;
        default = config.name;
      };

      dir = lib.mkOption {
        description = "";
        type = str;
        default = ''/tmp/hix-vm/${global.name}/${config.vm.name}'';
      };

      pidfile = lib.mkOption {
        type = str;
        description = "The file storing the qemu process' process ID.";
        default = "${config.vm.dir}/vm.pid";
      };

      image = lib.mkOption {
        type = str;
        description = "The path to the image file.";
        default = "${config.vm.dir}/vm.qcow2";
      };

      monitor = lib.mkOption {
        description = "The monitor socket for the VM.";
        type = str;
        default = "${config.vm.dir}/monitor";
      };

      system = lib.mkOption {
        description = "The system architecture string used for this VM, defaulting to [](#opt-general-system).";
        type = str;
        default = global.system;
      };

      headless = lib.mkOption {
        description = ''
        VMs are run without a graphical connection to their console.
        For debugging purposes, this option can be disabled to show the window.
        '';
        type = bool;
        default = true;
      };

      setup = lib.mkOption {
        description = "Commands for starting the VM.";
        type = str;
      };

      exit = lib.mkOption {
        description = "Commands for shutting down the VM.";
        type = str;
      };

      derivation = lib.mkOption {
        description = "The VM derivation";
        type = types.path;
      };

    };

    internal = {

      overridesLocal = lib.mkOption {
        description = "The local packages, encoded as overrides.";
        type = util.types.cabalOverridesVia "project";
      };

      overridesEnv = lib.mkOption {
        description = "Overrides for this env, including extras like managed dependencies.";
        type = util.types.cabalOverridesVia "computed env ${config.name}";
        readOnly = true;
      };

      overridesInherited = lib.mkOption {
        description = "The inherited overrides used for this env.";
        type = util.types.cabalOverridesVia "inherited by env ${config.name}";
        default = [];
      };

      overridesSolver = lib.mkOption {
        description = "Overrides for this env for use with the solver in managed dependency apps.";
        type = util.types.cabalOverridesVia "computed env ${config.name} solver";
        default = [];
      };

      resolvedServices = lib.mkOption {
        description = "Magic modules that allow merging env-specific service config with their base config.";
        type = types.attrsOf (types.submodule resolveServiceModule);
        readOnly = true;
      };

    };

  };

  config = {

    enable = lib.mkDefault true;

    services.hix-internal-env-wait.enable = config.wait > 0;

    services.ssh.enable = config.defaults;

    package-set = let

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

      compiler = lib.mkIf (config.compiler != null) (internal.modules.envDefault (
        if global.compilers ? ${config.compiler}
        then config.compiler
        else { source = config.compiler; }
      ));

      overrides = internal.modules.envDefault (util.concatOverrides overrideSources);

      gen-overrides = internal.modules.envDefault true;

    };

    toolchain = let
      duplicateCompiler =
        config.compiler != null &&
        config.package-set.compiler != config.compiler &&
        config.package-set.compiler != { source = config.compiler; };

      message = ''
      The configuration for 'envs.${name}' specifies both 'compiler' and 'package-set.compiler.source'.
      The former will be ignored.
      '';
    in internal.warn.warnEval duplicateCompiler "env.duplicate-compiler" message built.toolchain;

    ghc.toolchain = built.toolchain;

    hostPorts = util.mapListCatAttrs (s: lib.mapAttrs (_: effectiveHostPort) s.ports) built.resolvedServices;

    shell = lib.mkDefault (global.pkgs.stdenv.mkDerivation {
      inherit (config) name;
      inherit (built) buildInputs;
      shellHook = ''
      _hix_is_shell=1
      ${config.code}
      '';
    });

    ghcWithPackages =
      util.ghc.packageDbFull config ({ withHoogle = config.hoogle; } // config.ghcWithPackagesArgs);

    code = internal.env.scripts.setup { env = config; };

    ghcid.package = lib.mkDefault (ghcidMod built.toolchain.vanilla.ghcid);

    hls.package = lib.mkDefault built.toolchain.vanilla.haskell-language-server;

    vm = {

      enable = let
        builtInCount = (if config.wait > 0 then 1 else 0) + (if config.defaults then 1 else 0);
      in lib.mkDefault (lib.length built.resolvedServices > builtInCount);

      derivation = lib.mkDefault (
        let nixosArgs = {
          inherit (config.vm) system;
          modules = combinedConfig;
        };
        in (lib.nixosSystem nixosArgs).config.system.build.vm
        );

        setup = lib.mkDefault "${internal.vm.ensure config.basePort config.vm}";
        exit = lib.mkDefault "${internal.vm.kill config.vm}";

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

        overridesEnv = util.concatOverrides (
          lib.optional config.managed (internal.env.managedOverrides config.name)
          ++
          [config.overrides]
        );

        overridesInherited = util.unlessDevEnv config global.envs.dev.internal.overridesInherited;

        # Config is given by modules merged into the option declaration
        resolvedServices = lib.mapAttrs (_: _: {}) config.services;

      };

  };
}
