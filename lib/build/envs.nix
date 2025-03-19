{util}: let

  inherit (util) build internal lib;

  buildEnv = env: packages:
  internal.env.setWithMain (main: { inherit (packages.${main.name}) cross static musl release; }) env
  //
  {
    buildInputs = internal.env.buildInputs env;
    executables = lib.concatMapAttrs (_: outputs: outputs.executables) packages;
    resolvedServices =
      lib.filter (conf: conf.enable) (lib.mapAttrsToList (_: s: s.resolve) env.internal.resolvedServices);
  }
  ;

in internal.envs.map buildEnv (internal.envs.filterEnabled build.packages)
