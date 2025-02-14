{pkgs, ...}:
{
  source = ''
  output_match 'Hackage: 1.0.0'
  step_run show-overrides dev

  describe "stm-chans version override in 'root' supersedes the one from 'dep1'."
  output_exact '"2.0.0"'
  step_eval stm-chans-version.${pkgs.system}

  step_build root.min

  step_build

  describe 'Run the main package'
  output_exact 'success66'
  step result/bin/run

  step_run gen-cabal-quiet

  cabal_update()
  {
    output_ignore
    step_develop cabal update -v0
  }

  if_ci cabal_update
  error_match '3 files worked'
  step_run hls
  '';
}
