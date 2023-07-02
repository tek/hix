{ pkgs }:
{
  test = builtins.toFile "local-prelude-test" ''
    cd ./root
    nix flake update

    ghci_match ".#ghci -- -p root -t main" "success" 'local-prelude broken'
  '';
}
