{ pkgs }:
{
  test = builtins.toFile "subdir-test" ''
    cd ./root
    git init --quiet
    git add .
    cd sub
    nix flake update

    nix run .#gen-cabal
  '';
}
