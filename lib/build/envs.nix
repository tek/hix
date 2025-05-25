{util}: let

  inherit (util) build internal lib;

  buildEnv = env: packages: let

    package-set = internal.modules.resolveExtensibleModule "package-sets" env.package-set;

    toolchain = internal.package-sets.toToolchain package-set;

  in
  internal.env.setWithMain (main: { inherit (packages.${main.name}) cross static musl release; }) env
  //
  {

    buildInputs = internal.env.buildInputs env;

    executables = lib.concatMapAttrs (_: outputs: outputs.executables) packages;

    resolvedServices =
      lib.filter (conf: conf.enable) (lib.mapAttrsToList (_: s: s.resolve) env.internal.resolvedServices);

    inherit package-set toolchain;
    inherit (toolchain) pkgs;

  }
  ;

in internal.envs.map buildEnv build.packages
