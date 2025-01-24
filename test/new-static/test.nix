{...}:
{
  source = ''
    mkdir root
    cd ./root
    nix run "path:$hix_dir#new" -- --name 'red-panda' --author='Red Panda'
    nix --quiet --quiet run .#gen-cabal-quiet
    ghci_match '.#ghci -- -p red-panda -t main' 'passed 1 test' 'Running tests in generated project failed'
    check_match 'nix run' 'Hello red-panda' 'App in generated project failed'
  '';
}
