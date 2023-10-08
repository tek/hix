{ pkgs }:
{
  test = builtins.toFile "subdir-test" ''
    cd ./root
    git init --quiet
    git add .
    cd sub
    flake_update

    nix run .#gen-cabal-quiet
  '';
}
