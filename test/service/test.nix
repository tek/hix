{ pkgs }:
{
  test = builtins.toFile "service-test" ''
    cd ./root
    nix flake update
    check_match 'nix run .#cmd.test' 'received' 'invalid output of service command'
  '';
}
