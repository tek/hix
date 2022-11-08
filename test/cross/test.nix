{ pkgs }:
{
  test = builtins.toFile "cross-test" ''
    cd ./root
    nix flake update
    nix build .#static
    output=$(result/bin/run)
    if [[ $output != 'string' ]]
    then
      fail "Running the main package produced the wrong output:\n$output"
    fi
    if ! { ldd result/bin/run | grep musl &>/dev/null }
    then
      fail "Executable isn't linked against musl"
    fi
  '';
}
