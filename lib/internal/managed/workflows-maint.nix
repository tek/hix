{util}: let

  inherit (util) config;

  maint = config.pkgs.writeText "maint.yaml" ''
  name: Maintain release bounds

  on: workflow_dispatch

  permissions:
    contents: write
    pull-requests: write

  env:
    LANG: en_US.UTF-8

  jobs:
    maint-run:
      name: Update release bounds
      runs-on: ubuntu-latest
      outputs:
        results: ''${{ steps.maint.outputs.maint }}

      steps:

      - uses: actions/checkout@v4
        with:
          # This must be a PAT with workflows:write to allow pushing new release branches when the main branch has
          # modified any workflows.
          token: ''${{ secrets.workflow_token }}

      - uses: cachix/install-nix-action@v31
        with:
          github_access_token: ''${{ secrets.GITHUB_TOKEN }}

      - uses: cachix/cachix-action@v15
        with:
          name: hix-build

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
        run: >
          gh pr create
          --base "''${{ matrix.package.baseBranch }}"
          --body "''${{ matrix.package.message }}"
          --title "revision for ''${{ matrix.package.package }}"
          --head "''${{ matrix.package.branch }}"
        env:
          GH_TOKEN: ''${{ secrets.GITHUB_TOKEN }}
  '';

  revision = config.pkgs.writeText "revision.yaml" ''
  name: Publish revision for updated bounds

  on:
    workflow_dispatch:
    pull_request:
      types: [closed]
      branches: ['release/**']

  env:
    LANG: en_US.UTF-8
    branch: ''${{ github.event_name == 'workflow_dispatch' && github.ref_name || github.base_ref }}

  jobs:
    revision:
      name: Publish revision
      if: github.event.pull_request.merged == true || github.event_name == 'workflow_dispatch'
      runs-on: ubuntu-latest

      steps:

      - uses: actions/checkout@v4

      - uses: cachix/install-nix-action@v31
        with:
          github_access_token: ''${{ secrets.GITHUB_TOKEN }}

      - uses: cachix/cachix-action@v15
        with:
          name: hix-build

      - id: revision
        name: Publish revision
        run: >
          nix run .#revision -- --fetch
          --hackage='hackage.haskell.org:password:''${{ secrets.hackage_password }}'
          --branch "$branch"
  '';

in {

  inherit maint revision;

}
