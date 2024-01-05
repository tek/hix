{...}:
{
  test = builtins.toFile "cross-test" ''
    cd ./root
    flake_update

    check_musl()
    {
      nix build $1
      output=$(result/bin/exe)
      check 'result/bin/exe' 'string' "Running the musl binary for $1 produced the wrong output"
      if ! { ldd result/bin/exe | grep musl &>/dev/null }
      then
        fail "Executable for $1 isn't linked against musl"
      fi
    }

    check_musl ".#root.cross.musl64"

    check_musl ".#root.cross.musl64.static"

    check_musl ".#ghc94-root.cross.musl64.static"

    check_musl ".#env.cross.root.static"

    check_musl ".#env.cross.static"

    check_musl ".#env.cross.static-override"

    check_appimage()
    {
      check "$(nix run $1 2>/dev/null)" 'string' "Running the AppImage for $1 produced the wrong output"
    }

    check_appimage '.#appimage'

    check_appimage '.#root.appimage'

    check_appimage '.#alias.appimage'

    check_appimage '.#env.ghc94.appimage'

    check_appimage '.#env.ghc94.root.appimage'

    check_appimage '.#env.ghc94.exe.appimage'
  '';
}
