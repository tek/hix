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

    output="$(nix run .#ghci -- -p root -c lib -m Root.Lib -r print <<< ':quit')"
    if [[ ! $output =~ 'print success' ]]
    then
      fail "ghci test 1 does not contain 'print success'"
    fi

    check "$(nix run .#ghci -- -p root -t main <<< ':quit')"

    check "$(nix run .#env.test.ghci -- -p root -t main <<< ':quit')"
  '';
}
