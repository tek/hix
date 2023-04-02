{ pkgs }:
{
  test = builtins.toFile "service-test" ''
    cd ./root
    nix flake update
    output=$(nix run .#cmd.test)

    if [[ $output != 'received' ]]
    then
      fail "invalid output of service command:\n$output"
    fi
  '';
}
