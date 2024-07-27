{...}:
{
  test = builtins.toFile "new-test" ''
    mkdir root
    cd ./root
    nix run path:$hix_dir#cli -- new --hix-url="path:$hix_dir" --name 'red-panda' --author 'Panda'
    nix --quiet --quiet run .#gen-cabal-quiet
    ghci_match '.#ghci -- -p red-panda -t main' 'passed 1 test' 'Running tests in generated project failed'
    check_match 'nix run' 'Hello red-panda' 'App in generated project failed'
  '';
}
