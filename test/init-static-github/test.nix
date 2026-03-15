# This doesn't actually use static linking at the moment, since it's broken in nixpkgs.
{
  root = false;
  updateLock = false;

  source = ''
    mkdir root
    cd ./root

    step_nix run "github:tek/hix#cli" -- init --hix-url 'github:tek/hix' --name 'red-panda' --dev-cli

    step_run gen-cabal-quiet

    describe 'Run tests in generated project'
    output_match 'passed 1 test'
    error_ignore
    step_ghci ghci -p red-panda -t main

    describe 'Run app in generated project'
    output_match 'Hello red-panda'
    step_run
  '';
}
