{util}: let
  inherit (util) config lib internal;

  # TODO refactor this into the new outputs protocol
  appsWith = cons: {
    bump = cons { cmd = internal.managed.cmd.bump; env = "latest"; sub = null; };
    lower = {
      auto = cons { cmd = internal.managed.cmd.lower ""; env = "lower"; sub = "auto"; };
      init = cons { cmd = internal.managed.cmd.lower "init"; env = "lower"; sub = "init"; };
      optimize = cons { cmd = internal.managed.cmd.lower "optimize"; env = "lower"; sub = "optimize"; };
      stabilize = cons { cmd = internal.managed.cmd.lower "stabilize"; env = "lower"; sub = "stabilize"; };
    };
    # TODO maint should also be executable for individual sets
    maint = cons { cmd = _: internal.managed.cmd.maint; env = null; sub = null; };
    revision = cons { cmd = _: internal.managed.cmd.revision; env = null; sub = null; };
  };

  appsForAllEnvs = appsWith ({cmd, env, sub}: cmd []);

  appsForEnvs = envs: appsWith ({cmd, env, sub}: cmd [envs.${env}]);

  managedCmdMulti = sets: {cmd, env, ...}:
  lib.genAttrs sets (name: cmd ["${env}-${name}"]);

  managedMulti = sets: appsWith (managedCmdMulti sets);

  # We're not guarding this with a `optionalAttrs` so that we can print an error when the user executes an app.
  # TODO Add an override option that opts into removing these apps from the flake.
  mixedApps = let
    sets = config.managed.sets;

    specific =
    if sets == "all"
    then {}
    else if sets == "each"
    then managedMulti internal.project.packageNames
    else if lib.isAttrs sets
    then managedMulti (lib.attrNames sets)
    else throw "Unexpected value for 'managed.sets': ${lib.generators.toPretty sets}"
    ;
  in util.mergeAuto specific appsForAllEnvs;

  legacyApps = {
    inherit (mixedApps) bump lower;
  };

  apps = util.mapValues util.app {
    inherit (appsForAllEnvs) bump maint revision;
    lower = appsForAllEnvs.lower.auto;
  };

  scopedAppsForEnvs = envs: let

    base = appsForEnvs envs;

  in base // {
    lower = base.lower.auto // base.lower;
  };

  workflowsManaged = import ./workflows-managed.nix { inherit util; };
  workflowsMaint = import ./workflows-maint.nix { inherit util; };
  workflowsRelease = import ./workflows-release.nix { inherit util; };

  genGaWorkflow = {kind, config}:
  util.scriptBin "gen-managed-ga-workflow" ''
  dir="$PWD/.github/workflows"
  target="$dir/${kind}.yaml"
  mkdir -p $dir
  cp ${config} $target
  chmod 600 $target
  '';

  genGaWorkflowPr = args:
  genGaWorkflow { inherit (args) kind; config = workflowsManaged.boundsPr args; };

  gen = {
    managed = {
      gen.ga = {
        bump = genGaWorkflowPr { kind = "bump"; name = "Bump upper bounds"; };
        lower = genGaWorkflowPr { kind = "lower"; name = "Manage lower bounds"; };
        maint = genGaWorkflow { kind = "maint"; config = workflowsMaint.maint; };
        revision = genGaWorkflow { kind = "revision"; config = workflowsMaint.revision; };
        releaseCandidates = genGaWorkflow { kind = "candidates"; config = workflowsRelease.candidates; };
        releasePublish = genGaWorkflow { kind = "publish"; config = workflowsRelease.publish; };
      };
    };
  };

  envGhcs = lib.mapAttrs (_: env: { ghc-local = internal.managed.managedEnvGhc env; }) internal.managed.env.envs;

in {
  inherit scopedAppsForEnvs legacyApps apps gen envGhcs;
}
