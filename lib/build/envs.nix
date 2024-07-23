{util}: let

  inherit (util) build internal lib;

  buildEnv = env: packages:
  internal.env.setWithMain (main: { inherit (packages.${main.name}) cross static musl release; }) env
  //
  { executables = lib.concatMapAttrs (_: outputs: outputs.executables) packages; }
  ;

in internal.envs.mapMaybe buildEnv build.packages
