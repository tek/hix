{ pkgs }:
{
  test = builtins.toFile "ghci-test" ''
    cd ./root
    nix flake update
    nix run .#hpack

    output=$(nix run .#ghci -- -p root -t main <<< ':quit')

    if [[ ! $output =~ 'two modules loaded' ]] || [[ ! $output =~ 'test-endpoint' ]]
    then
      fail "Failure in ghci:\n$output"
    fi
  '';
}
