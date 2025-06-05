{util}: let

  inherit (util) config;

  # TODO use this instead of the action.
  # - id: pr
  #   name: Create PR
  #   run: >
  #     gh pr create
  #     --base "''${{ matrix.package.baseBranch }}"
  #     --body "''${{ matrix.package.message }}"
  #     --title "revision for ''${{ matrix.package.package }}"
  #     --head "''${{ matrix.package.branch }}"
  #   env:
  #     GH_TOKEN: ''${{ secrets.GITHUB_TOKEN }}

  boundsPr = {kind, name}: config.pkgs.writeText "${kind}.yaml" ''
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

      - uses: cachix/install-nix-action@v31
        with:
          github_access_token: ''${{ secrets.GITHUB_TOKEN }}

      - id: bounds
        name: Update managed bounds
        run: nix run .#${kind} -- --output=ga-pr

      - id: pr
        name: Create pull request
        if: steps.bounds.outputs.commit-message
        uses: peter-evans/create-pull-request@v5
        with: ''${{ steps.bounds.outputs }}
  '';
in {
  inherit boundsPr;
}
