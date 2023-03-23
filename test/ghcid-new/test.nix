{ pkgs }:
{
  test = builtins.toFile "ghcid-test" ''
    cd ./root
    nix flake update

    nix run .#ghcid-new

  '';
}
