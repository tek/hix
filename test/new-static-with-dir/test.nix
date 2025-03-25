{
  root = false;
  updateLock = false;

  source = ''
    mkdir root
    cd ./root

    step_nix run "path:$hix_dir#new" -- --name 'red-panda' --author='Red Panda' --directory

    cd ./red-panda

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
