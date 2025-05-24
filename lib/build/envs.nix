{util}: let

  inherit (util) config internal lib;

  buildEnv = env: let

    package-set = internal.modules.resolveExtensibleModule "package-sets" env.package-set;

    toolchain = internal.package-sets.toToolchain package-set;

  in {

    inherit (env) name shell packages;

    validate = purpose: internal.env.validate purpose env;

    buildInputs = internal.env.buildInputs env;

    resolvedServices =
      lib.filter (conf: conf.enable) (lib.mapAttrsToList (_: s: s.resolve) env.internal.resolvedServices);

    inherit package-set toolchain;
    inherit (toolchain) pkgs;

  };

in util.mapValues buildEnv config.envs
