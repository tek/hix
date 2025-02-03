{util}: let
  inherit (util) config lib app;

  appsWith = cons: {
    bump = cons { cmd = util.managed.cmd.bump; env = "latest"; sub = null; };
    lower = cons { cmd = util.managed.cmd.lower ""; env = "lower"; sub = null; } // {
      auto = cons { cmd = util.managed.cmd.lower "auto"; env = "lower"; sub = "auto"; };
      init = cons { cmd = util.managed.cmd.lower "init"; env = "lower"; sub = "init"; };
      optimize = cons { cmd = util.managed.cmd.lower "optimize"; env = "lower"; sub = "optimize"; };
      stabilize = cons { cmd = util.managed.cmd.lower "stabilize"; env = "lower"; sub = "stabilize"; };
    };
  };

  appsForEnvs = envs: appsWith ({cmd, env, sub}: util.app (cmd [envs.${env}]));

  managedCmdMulti = envSort: cmd: sets: let
    envName = name: "${envSort}-${name}";
    envs = map (name: envName name) sets;
  in
  lib.genAttrs sets (name: app (cmd [(envName name)])) //
  app (cmd envs);

  managedMulti = sets: appsWith ({cmd, env, ...}: managedCmdMulti env cmd sets);

  # We're not guarding this with a `optionalAttrs` so that we can print an error when the user executes an app.
  # TODO Add an override option that opts into removing these apps from the flake.
  apps = let
    sets = config.managed.sets;
  in
    if sets == "all"
    then appsForEnvs { latest = "latest"; lower = "lower"; }
    else if sets == "each"
    then managedMulti config.internal.packageNames
    else if lib.isAttrs sets
    then managedMulti (lib.attrNames sets)
    else throw "Unexpected value for 'managed.sets': ${lib.generators.toPretty sets}"
    ;

  gaWorkflow = sort: config.pkgs.writeText "${sort}.yaml" ''
    name: ${sort}
    on: workflow_dispatch
    permissions:
      contents: write
      pull-requests: write
    jobs:
      ${sort}-pr:
        runs-on: ubuntu-latest
        steps:
        - uses: actions/checkout@v4
        - uses: DeterminateSystems/nix-installer-action@main
          with:
            extra-conf: |
              access-tokens = github.com=''${{ secrets.GITHUB_TOKEN }}
        - id: bounds
          run: nix run .#${sort} -- --output=ga-pr
        - name: pr
          if: steps.bounds.outputs.commit-message
          uses: peter-evans/create-pull-request@v5
          with: ''${{ steps.bounds.outputs }}
  '';

  genGaWorkflow = sort: let
    script = util.script "gen-managed-ga-workflow" ''
    dir="$PWD/.github/workflows"
    target="$dir/${sort}.yaml"
    mkdir -p $dir
    cp ${gaWorkflow sort} $target
    chmod 600 $target
    '';
  in app script;

  gen = {
    managed = {
      gen.ga = {
        bump = genGaWorkflow "bump";
        lower = genGaWorkflow "lower";
      };
    };
  };

  envGhcs = lib.mapAttrs (_: env: { ghc-local = util.managed.managedEnvGhc env; }) util.managed.env.envs;

in {
  inherit appsForEnvs apps gen envGhcs;
}
