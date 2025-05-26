{
  genCabal = true;

  source = ''
  describe 'GHC library visible'
  output_match 'one module loaded'
  step_ghci ghci -p api -m Root.LibGhc

  describe 'Test dep exposed in package DB'
  output_ignore
  step_ghci ghci -p api -c test

  describe 'GHCi changes directory'
  output_match "$PWD/pkg/"
  step_ghci ghci --root $PWD -c lib -m Root.Lib -r cwd

  describe 'GHCi does not change directory with --no-cd'
  output_match "$PWD/"
  step_ghci ghci -c lib -m Root.Lib -r cwd --no-cd

  describe "GHCi 'print' runner"
  output_match 'print success'
  step_ghci ghci -p api -c lib -m Root.Lib -r print

  describe 'Run ghci-app'
  output_match 'print success'
  step_ghci ghci-app

  describe 'Module in source dir at package/project root'
  output_match 'success'
  step_ghci ghci -p ./. -m Main -t main -c root
  '';
}
