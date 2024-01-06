{...}:
{
  test = builtins.toFile "env-test" ''
    cd ./root
    flake_update

    # TODO this can be 1 when a solution for component env fallback is implemented
    check 'nix run .#cmd.number' 2 'Wrong output for plain command with component'
    check 'nix run .#cmd.number-nocomp' 1 'Wrong output for plain command without component'
    check 'nix run .#cmd.number -- -p root -c app' 2 'Wrong output for command with component selection'
    check "nix run .#cmd.number -- -f $PWD/app/Main.hs" 2 'Wrong output for command with file selection'
    check 'nix run .#env.three.number-nocomp --' 3 'Wrong output for command with env selection via flake app attr'
    check 'nix run .#cmd.run -- "ghc --version"' 'The Glorious Glasgow Haskell Compilation System, version 9.4.7' 'Wrong output for run command'
    check 'nix run .#cmd.run -- "cabal --version 2>&1 | head"' 'cabal-install version 3.10.1.0' 'Wrong output for run command'
  '';
}
