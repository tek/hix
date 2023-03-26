{ pkgs }:
{
  test = builtins.toFile "ghci-test" ''
    cd ./root
    nix flake update
    nix run .#hpack

    output=$(nix run .#ghci -- -p root -t main <<< ':quit')

    if [[ ! $output =~ 'one module loaded' ]] || [[ ! $output =~ 'ghci test success' ]]
    then
      fail "Failure in ghci:\n$output"
    fi
  '';
}
