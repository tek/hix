{
  genCabal = true;

  source = ''
  describe 'GHC library visible'
  output_match 'one module loaded'
  step_ghci ghci -p root -m Root.LibGhc

  describe 'GHCi changes directory'
  output_match "$PWD/pkg/"
  step_ghci ghci --root $PWD -c lib -m Root.Lib -r cwd

  describe 'GHCi does not change directory with --no-cd'
  output_match "$PWD/"
  step_ghci ghci -- -c lib -m Root.Lib -r cwd --no-cd

  describe "GHCi 'print' runner"
  output_match 'print success'
  step_ghci ghci -p root -c lib -m Root.Lib -r print

  describe 'Run ghci-app'
  output_match 'print success'
  step_ghci ghci-app
  '';
}
