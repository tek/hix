{ pkgs }:
{
  test = builtins.toFile "bootstrap-test" ''
    cd ./root
    nix run path:$hix_dir#cli -- bootstrap --hix-url="path:$hix_dir"
    check_match 'nix run .#ghci -- -p red-panda -t main <<< :quit' 'success' 'Running tests in generated project failed'
  '';
}
