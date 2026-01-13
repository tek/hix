let
  args = "--root $PWD --build-handlers test --hackage hackage.haskell.org:indexState:2024-01-01T00:00:00Z aeson extra path";
in {
  git = true;

  source = ''
  file_exact ${./workflow.yaml} .github/workflows/bump.yaml
  step_run managed.gen.ga.bump

  output_exact '[ "dev-local1" "dev-local2" "dev-local3" "latest-main-local1" "latest-other-local2" "latest-other-local3" ]'
  step_output_names checks.x86_64-linux

  describe 'Batch run for commit message (state.nix)'
  file_exact ${./state.nix} 'ops/managed.nix'
  output_exact ${./commit-msg}
  step_run bump ${args} --output=commit-msg

  step git reset --hard --quiet

  sub_timestamp()
  {
    sed 's#hix-managed/bump-.*#hix-managed/bump-0#' $*
  }

  export GITHUB_OUTPUT="$PWD/github-output-out"
  describe 'Github actions PR output'
  file_exact ${./github-output} $GITHUB_OUTPUT
  preproc_files sub_timestamp
  step_run bump ${args} --output=ga-pr

  step git reset --hard --quiet

  describe 'Env run (state-main.nix)'
  file_exact ${./state-main.nix} 'ops/managed.nix'
  file_exact ${./local1.cabal} 'packages/local1/local1.cabal'
  file_exact ${./local3.cabal} 'packages/local3/local3.cabal'
  step_run env.latest-main.bump ${args}
  '';
}
