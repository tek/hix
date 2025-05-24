{util}: let

  inherit (util) config build lib;

  depVersions = env: import ../dep-versions.nix { inherit config lib util env; };

  legacyEnv = env:
  lib.optionalAttrs util.expose.internals {
    inherit (env.toolchain) pkgs;
    ghc = env.toolchain.packages;
    ghc0 = env.toolchain.vanilla;
  };

  appsEnv = env: { dep-versions = depVersions env.name; inherit (env) shell; };

  managedEnv = env: {
    solver = util.ghc.packageDbSolver (!config.managed.internal.localsInPackageDb) env;
  };

  prefixedEnv = name: env:
  legacyEnv env // appsEnv env;

  prefixed.env = mapValidated prefixedEnv build.envs;

  errorStub = text: name: let
    desc = "hix-env-${name}-not-exposed";
    main = util.hixScript desc {} text;
  in (util.zscriptErr "${desc}-with-shell" main).overrideAttrs {
    shellHook = ''
    ${main}
    exit 1
    '';
  };

  disabled = name: let
    text = ''
    error_message "The environment $(color_env ${name}) is disabled because the option \
    $(color_option "envs.${name}.enable") is set to $(blue 'false')."
    '';
  in errorStub text name;

  disallowedSystem = name: let
    text = ''
    error_message "The environment $(color_env ${name}) is disabled because $(color_option "envs.${name}.systems") is \
    set and does not contain the current system, $(green ${config.system})."
    '';
  in errorStub text name;

  notExposed = purpose: name: let
    text = ''
    error_message "The environment $(color_env ${name}) is configured not to be exposed at this flake output."
    error_message "You can enable it by setting $(blue "envs.${name}.expose.${purpose} = true;")"
    '';
  in errorStub text name;

  wrapValidated = purpose: f: env: let
    validation = env.validate purpose;
  in if !validation.enable
  then disabled env.name
  else if !validation.system
  then disallowedSystem env.name
  else if !validation.exposed
  then notExposed purpose env.name
  else f env
  ;

  mapValidated = purpose: f:
  util.mapValues (wrapValidated purpose f);

in {

  legacyPackages =
    prefixed
    //
    mapValidated "scoped" appsEnv build.envs
    ;

  shells = mapValidated "shell" (e: e.shell) build.envs;

  internal.env = util.mapValues managedEnv util.managed.env.envs;

}
