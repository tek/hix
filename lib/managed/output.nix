{util}: let
  inherit (util) config lib internal;

  # TODO refactor this into the new outputs protocol
  appsWith = cons: {
    bump = cons { cmd = util.managed.cmd.bump; env = "latest"; sub = null; };
    lower = {
      auto = cons { cmd = util.managed.cmd.lower ""; env = "lower"; sub = "auto"; };
      init = cons { cmd = util.managed.cmd.lower "init"; env = "lower"; sub = "init"; };
      optimize = cons { cmd = util.managed.cmd.lower "optimize"; env = "lower"; sub = "optimize"; };
      stabilize = cons { cmd = util.managed.cmd.lower "stabilize"; env = "lower"; sub = "stabilize"; };
    };
    # TODO maint should also be executable for individual sets
    maint = cons { cmd = _: util.managed.cmd.maint; env = null; sub = null; };
    revision = cons { cmd = _: util.managed.cmd.revision; env = null; sub = null; };
  };

  appsForEnvs = envs: appsWith ({cmd, env, sub}: cmd [envs.${env}]);

  managedCmdMulti = envSort: cmd: sets: let
    envName = name: "${envSort}-${name}";
    envs = map (name: envName name) sets;
  in
  lib.genAttrs sets (name: cmd [(envName name)]) //
  cmd envs;

  managedMulti = sets: appsWith ({cmd, env, ...}: managedCmdMulti env cmd sets);

  # We're not guarding this with a `optionalAttrs` so that we can print an error when the user executes an app.
  # TODO Add an override option that opts into removing these apps from the flake.
  mixedApps = let
    sets = config.managed.sets;
  in
    if sets == "all"
    then appsForEnvs { latest = "latest"; lower = "lower"; }
    else if sets == "each"
    then managedMulti internal.project.packageNames
    else if lib.isAttrs sets
    then managedMulti (lib.attrNames sets)
    else throw "Unexpected value for 'managed.sets': ${lib.generators.toPretty sets}"
    ;

  legacyApps.lower = mixedApps.lower;

  apps = util.mapValues util.app {
    inherit (mixedApps) bump maint revision;
    lower = mixedApps.lower.auto;
  };

  scopedAppsForEnvs = envs: let

    base = appsForEnvs envs;

  in base // {
    lower = base.lower.auto // base.lower;
  };

    # TODO use this instead of the action. Optimally, change it to use json output like the maint flow
    # - id: pr
    #   name: Create pull request
    #   run: |
    #     git switch --create "$branch"
    #     git add .
    #     git commit -m "$message"
    #     git push origin "$branch"
    #     gh pr create --base "''${{ github.ref }}" --head "$branch" --body "$message" --title "''${{ steps.bounds.outputs.title }}"
    #   env:
    #     GH_TOKEN: ''${{ secrets.GITHUB_TOKEN }}
    #     branch: ''${{ steps.bounds.outputs.branch }}
    #     message: ''${{ steps.bounds.outputs.commit-message }}

  gaWorkflowPr = {kind, name}: config.pkgs.writeText "${kind}.yaml" ''
  name: ${name}

  on: workflow_dispatch

  permissions:
    contents: write
    pull-requests: write

  jobs:
    ${kind}-pr:
      name: Create PR for updated bounds
      runs-on: ubuntu-latest
      steps:

      - uses: actions/checkout@v4

      - uses: DeterminateSystems/nix-installer-action@v16
        with:
          github-token: ''${{ secrets.GITHUB_TOKEN }}

      - id: bounds
        name: Update managed bounds
        run: nix run .#${kind} -- --output=ga-pr

      - id: pr
        name: Create pull request
        if: steps.bounds.outputs.commit-message
        uses: peter-evans/create-pull-request@v5
        with: ''${{ steps.bounds.outputs }}
  '';

  gaWorkflowMaint = config.pkgs.writeText "maint.yaml" ''
  name: Maintain release bounds

  on: workflow_dispatch

  permissions:
    contents: write
    pull-requests: write

  jobs:
    maint-run:
      name: Update release bounds
      runs-on: ubuntu-latest
      outputs:
        results: ''${{ steps.maint.outputs.maint }}

      steps:
      - uses: actions/checkout@v4
      - uses: DeterminateSystems/nix-installer-action@v16
        with:
          github-token: ''${{ secrets.GITHUB_TOKEN }}
      - uses: cachix/cachix-action@v15
        with:
          name: tek
      - id: maint
        name: Run maint
        run: |
          nix run .#maint -- --pr --fetch --output=json --target=github

    maint-pr:
      name: Create PR for release bounds updates
      runs-on: ubuntu-latest
      needs: maint-run
      strategy:
        matrix:
          package: ''${{ fromJSON(needs.maint-run.outputs.results).changes }}
      steps:
      - uses: actions/checkout@v4
      - id: pr
        name: Create PR
        run: |
          gh pr create \
            --base "''${{ matrix.package.baseBranch }}" \
            --body "''${{ matrix.package.message }}" \
            --title "revision for ''${{ matrix.package.package }}" \
            --head "''${{ matrix.package.branch }}"
        env:
          GH_TOKEN: ''${{ secrets.GITHUB_TOKEN }}
  '';

  gaWorkflowRevision = config.pkgs.writeText "revision.yaml" ''
  name: Publish revision for updated bounds

  on:
    pull_request:
      types: [closed]
      branches: ['release/**']

  jobs:
    revision:
      name: Publish revision
      if: github.event.pull_request.merged == true
      runs-on: ubuntu-latest
      steps:
      - uses: actions/checkout@v4
      - uses: DeterminateSystems/nix-installer-action@v16
        with:
          github-token: ''${{ secrets.GITHUB_TOKEN }}
      - uses: cachix/cachix-action@v15
        with:
          name: tek
      - id: revision
        name: Publish revision
        run: |
          nix run .#revision -- --fetch \
            --hackage="hackage.haskell.org:password:''${{ secrets.hackage_password }}" \
            --branch "''${{ github.base_ref }}"
  '';

  genGaWorkflow = {kind, config}:
  util.scriptBin "gen-managed-ga-workflow" ''
  dir="$PWD/.github/workflows"
  target="$dir/${kind}.yaml"
  mkdir -p $dir
  cp ${config} $target
  chmod 600 $target
  '';

  genGaWorkflowPr = args:
  genGaWorkflow { inherit (args) kind; config = gaWorkflowPr args; };

  gen = {
    managed = {
      gen.ga = {
        bump = genGaWorkflowPr { kind = "bump"; name = "Bump upper bounds"; };
        lower = genGaWorkflowPr { kind = "lower"; name = "Manage lower bounds"; };
        maint = genGaWorkflow { kind = "maint"; config = gaWorkflowMaint; };
        revision = genGaWorkflow { kind = "revision"; config = gaWorkflowRevision; };
      };
    };
  };

  envGhcs = lib.mapAttrs (_: env: { ghc-local = util.managed.managedEnvGhc env; }) util.managed.env.envs;

in {
  inherit scopedAppsForEnvs legacyApps apps gen envGhcs;
}
