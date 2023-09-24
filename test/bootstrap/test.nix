{ pkgs }:
{
  test = builtins.toFile "bootstrap-test" ''
    cd ./root
    check_match 'nix run path:$hix_dir#bootstrap' 'Initialized' "Bootstrap command didn't initialize git repo"
    check_match 'nix run .#ghci -- -p red-panda -t main <<< :quit' 'success' 'Running tests in generated project failed'
  '';
}
