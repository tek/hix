{...}:
{
  test = builtins.toFile "ghci-test" ''
    cd ./root
    flake_update
    nix run .#gen-cabal-quiet

    ghci_match '.#ghci -- -p root -m Root.LibGhc' 'one module loaded' 'ghc library not visible'

    ghci_match ".#ghci -- --root $PWD -c lib -m Root.Lib -r cwd" "$PWD/pkg/" 'ghci cwd with cd printed wrong directory'

    ghci_match '.#ghci -- -c lib -m Root.Lib -r cwd -- --no-cd' "$PWD/" 'ghci cwd with no-cd printed wrong directory'

    ghci_match '.#ghci -- -p root -c lib -m Root.Lib -r print' 'print success' "ghci output for 'print' runner does not contain 'print success'"

    ghci_match '.#ghci-app' 'print success' "ghci-app did not output 'print success'"
  '';
}
