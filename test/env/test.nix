{ pkgs }:
{
  test = builtins.toFile "env-test" ''
    cd ./root
    nix flake update
    nix run .#gen-cabal

    check 'nix run .#cmd.number' 1 'Wrong output for plain command'
    check 'nix run .#cmd.number -- -p root -c app' 2 'Wrong output for command with component selection'
    check "nix run .#cmd.number -- -f $PWD/app/Main.hs" 2 'Wrong output for command with file selection'
    check 'nix run .#env.three.number --' 3 'Wrong output for command with env selection via flake app attr'
    check 'nix run .#cmd.run -- "ghc --version"' 'The Glorious Glasgow Haskell Compilation System, version 9.2.5' 'Wrong output for run command'
  '';
}
