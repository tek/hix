{ pkgs }:
{
  test = builtins.toFile "local-prelude-test" ''
    cd ./root
    flake_update

    ghci_match ".#ghci -- -p root -t main" "success" 'local-prelude broken'
  '';
}
