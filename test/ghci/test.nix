{ pkgs }:
{
  test = builtins.toFile "ghci-test" ''
    cd ./root
    nix flake update
    nix run .#hpack

    check()
    {
      local output=$1
      if [[ ! $output =~ 'two modules loaded' ]] || [[ ! $output =~ 'test-endpoint' ]]
      then
        fail "Failure in ghci:\n$output"
      fi
    }

    check "$(nix run .#c.ghci -- -p root -t main <<< ':quit')"

    check "$(nix run .#env.test.ghci -- -p root -t main <<< ':quit')"
  '';
}
