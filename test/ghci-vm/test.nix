{ pkgs }:
{
  test = builtins.toFile "ghci-vm-test" ''
    cd ./root
    flake_update
    nix run .#gen-cabal-quiet

    ghci_match '.#ghci -- -p root -t main' 'test-endpoint' "ghci service output with component env does not contain 'test-endpoint'"

    ghci_match '.#env.hix-ghci-test.ghci -- -t main' 'test-endpoint' "ghci service output with explicit env does not contain 'test-endpoint'"
  '';
}
