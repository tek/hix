{ pkgs }:
{
  test = builtins.toFile "auto-4-test" ''
    cd ./root
    nix flake update
    nix build
    output=$(result/bin/root)
    if [[ $output != 'string' ]]
    then
      fail "Running the main package with 'build' produced the wrong output:\n$output"
    fi
  '';
}
