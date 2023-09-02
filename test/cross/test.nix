{ pkgs }:
{
  test = builtins.toFile "cross-test" ''
    cd ./root
    nix flake update

    check_musl()
    {
      nix build $1
      output=$(result/bin/root)
      if [[ $output != 'string' ]]
      then
        fail "Running the main package for $1 produced the wrong output:\n$output"
      fi
      if ! { ldd result/bin/root | grep musl &>/dev/null }
      then
        fail "Executable isn't linked against musl for $1"
      fi
    }

    check_musl ".#root.cross.musl64"

    check_musl ".#root.cross.musl64.static"

    check_musl ".#ghc94-root.cross.musl64.static"

    check_musl ".#static"
  '';
}
