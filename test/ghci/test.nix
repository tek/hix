{ pkgs }:
{
  test = builtins.toFile "ghci-test" ''
    cd ./root
    nix flake update
    nix run .#gen-cabal

    ghci_match()
    {
      check_match "nix run $1 <<< ':quit'" $2 $3
    }

    ghci_match ".#ghci -- --root $PWD -c lib -m Root.Lib -r cwd" "$PWD/pkg/" 'ghci cwd with cd printed wrong directory'

    ghci_match '.#ghci -- -c lib -m Root.Lib -r cwd -- --no-cd' "$PWD/" 'ghci cwd with no-cd printed wrong directory'

    ghci_match '.#ghci -- -p root -c lib -m Root.Lib -r print' 'print success' "ghci output for 'print' runner does not contain 'print success'"

    ghci_match '.#ghci -- -p root -t main' 'test-endpoint' "ghci service output with component env does not contain 'test-endpoint'"

    ghci_match '.#env.hix-ghci-test.ghci -- -t main' 'test-endpoint' "ghci service output with explicit env does not contain 'test-endpoint'"
  '';
}
