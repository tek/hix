{ pkgs }:
{
  test = builtins.toFile "env-test" ''
    cd ./root
    nix flake update
    nix run .#gen-cabal

    check 'nix run .#cmd.number' 1 'Wrong output for plain command'
    check 'nix run .#cmd.number -- -p root -d app' 2 'Wrong output for command with component selection'
    check "nix run .#cmd.number -- -f $PWD/app/Main.hs" 2 'Wrong output for command with file selection'
    check 'nix run .#env.three.number --' 3 'Wrong output for command with env selection via flake app attr'
    check 'nix run .#cmd.run -- "ghc --version"' 'foo' 'Wrong output for run command'
  '';
}
