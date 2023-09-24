{...}:
{
  test = builtins.toFile "bootstrap-test" ''
    cd ./root
    git init
    check_match 'nix run path:$hix_dir#bootstrap' 'generated red-panda-core' "Bootstrap command didn't generate cabal files"
    check_match 'nix run .#ghci -- -p red-panda -t main <<< :quit' 'success' 'Running tests in generated project failed'
  '';
}
