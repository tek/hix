{util}: let
  inherit (util) config lib app;

  appsWith = cons: {
    bump = cons { cmd = util.managed.cmd.bump; env = "latest"; sub = null; };
    lower = {
      init = cons { cmd = util.managed.cmd.lower "init"; env = "lower"; sub = "init"; };
      optimize = cons { cmd = util.managed.cmd.lower "optimize"; env = "lower"; sub = "optimize"; };
      stabilize = cons { cmd = util.managed.cmd.lower "stabilize"; env = "lower"; sub = "stabilize"; };
    };
  };

  appsAll = envs: appsWith ({cmd, env, sub}: util.app (cmd [envs.${env}]));

  wantChecks = config.managed.enable && config.managed.check;

  prefixedEnvDerivations = env:
  lib.mapAttrs' (n: d: { name = "${env}-${n}"; value = d; }) config.envs.${env}.derivations;

  checks =
    lib.optionalAttrs wantChecks
    (util.foldMapAttrs prefixedEnvDerivations (lib.attrNames util.managed.env.envs))
    ;

  managedCmdMulti = envSort: cmd: sets: let
    envName = name: "${envSort}-${name}";
    envs = map (name: envName name) sets;
  in
  lib.genAttrs sets (name: app (cmd [(envName name)])) //
  app (cmd envs);

  managedMulti = sets: appsWith ({cmd, env, ...}: managedCmdMulti env cmd sets);

  # We're not guarding this with a `optionalAttrs` so that we can print an error when the user executes an app.
  # TODO Add an override option that opts into removing these apps from the flake.
  apps =
    if config.managed.sets == "all"
    then appsAll { latest = "latest"; lower = "lower"; }
    else if config.managed.sets == "each"
    then managedMulti config.internal.packageNames
    else managedMulti (lib.attrNames config.managed.sets)
    ;

  gaWorkflow = sort: config.pkgs.writeText "${sort}.yaml" ''
    name: ${sort}
    on: workflow_dispatch
    permissions:
      contents: write
      pull-requests: write
    jobs:
      bump-pr:
        runs-on: ubuntu-latest
        steps:
        - uses: actions/checkout@v4
        - uses: DeterminateSystems/nix-installer-action@main
          with:
            extra-conf: |
              access-tokens = github.com=''${{ secrets.GITHUB_TOKEN }}
        - uses: DeterminateSystems/magic-nix-cache-action@main
        - id: bump
          run: nix run .#${sort} -- --output=ga-pr
        - name: pr
          if: steps.bump.outputs.commit-message
          uses: peter-evans/create-pull-request@v5
          with: ''${{ steps.bump.outputs }}
  '';

  genGaWorkflow = sort: let
    script = config.pkgs.writeScript "gen-managed-ga-workflow" ''
    dir=$PWD/.github/workflows
    mkdir -p $dir
    cp ${gaWorkflow sort} $dir/${sort}.yaml
    '';
  in app script;

  gen = {
    managed = {
      gen.ga = {
        bump = genGaWorkflow "bump";
        lower.optimize = genGaWorkflow "lower.optimize";
      };
    };
  };

  envGhcs = lib.mapAttrs (_: env: { ghc-local = util.managed.managedEnvGhc env; }) util.managed.env.envs;

in {
  inherit appsAll checks apps gen envGhcs;
}
