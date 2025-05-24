{util}: let

  inherit (util) config internal lib;

  systemAllowed = env:
  env.systems == null || lib.elem config.system env.systems;

  validate = purpose: env: let
    system = systemAllowed env;
    exposed = internal.env.isExposed purpose env;
  in { valid = env.enable && system && exposed; inherit (env) enable name; inherit system exposed purpose; };

  buildEnv = env: let

    package-set = internal.modules.resolveExtensibleModule "package-sets" env.package-set;

    toolchain = internal.package-sets.toToolchain package-set;

  in {

    inherit (env) name shell packages;

    validate = purpose: validate purpose env;

    buildInputs = internal.env.buildInputs env;

    resolvedServices =
      lib.filter (conf: conf.enable) (lib.mapAttrsToList (_: s: s.resolve) env.internal.resolvedServices);

    inherit package-set toolchain;
    inherit (toolchain) pkgs;

  };

in util.mapValues buildEnv config.envs
