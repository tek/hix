{util}: let

  inherit (util) config;

  candidates = config.pkgs.writeText "candidates.yaml" ''
  name: Upload release candidates to Hackage

  on: workflow_dispatch

  permissions:
    contents: write
    pull-requests: write

  env:
    LANG: en_US.UTF-8

  jobs:
    candidates:
      name: Upload candidates
      runs-on: ubuntu-latest
      outputs:
        results: ''${{ steps.upload.outputs.release }}

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
          name: tek

      - id: upload
        name: Upload candidates
        run: >
          nix run .#cli -- hackage release
          --output=json
          --target=github
          --hackage='hackage.haskell.org:password:''${{ secrets.hackage_password }}'
          --version=major
          --check
          --candidates
          --commit
          --push
          --trace

    pr:
      name:  Create PR
      runs-on: ubuntu-latest
      needs: candidates
      strategy:
        matrix:
          include:
            - results: ''${{ fromJSON(needs.candidates.outputs.results) }}

      steps:

      - uses: actions/checkout@v4

      - id: pr
        name: Create PR
        env:
          GH_TOKEN: ''${{ secrets.GITHUB_TOKEN }}
        if: ''${{ matrix.results.success }}
        run: >
          gh pr create
          --base main
          --body "''${{ matrix.results.message }}"
          --title "''${{ matrix.results.title }}"
          --head "''${{ matrix.results.branch }}"
    '';

  publish = config.pkgs.writeText "publish.yaml" ''
  name: Publish packages to Hackage

  on:
    workflow_dispatch:
    pull_request:
      types: [closed]
      branches: ['main']

  permissions:
    contents: write

  env:
    LANG: en_US.UTF-8
    branch: ''${{ github.event_name == 'workflow_dispatch' && github.ref_name || github.base_ref }}

  jobs:
    publish:
      name: Publish packages
      if: (github.event.pull_request.merged == true && startsWith(github.head_ref, 'release-prepare-')) || github.event_name == 'workflow_dispatch'
      runs-on: ubuntu-latest

      steps:

      - uses: actions/checkout@v4

      - uses: cachix/install-nix-action@v31
        with:
          github_access_token: ''${{ secrets.GITHUB_TOKEN }}

      - uses: cachix/cachix-action@v15
        with:
          name: tek

      - id: publish
        name: Publish packages
        run: >
          nix run .#cli -- hackage release
          --hackage='hackage.haskell.org:password:''${{ secrets.hackage_password }}'
          --version=keep
          --candidates=none
          --publish
          --tag
          --push
          --trace
    '';

in {

  inherit candidates publish;

}
