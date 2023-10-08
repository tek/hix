{ pkgs }:
{
  test = builtins.toFile "service-test" ''
    cd ./root
    flake_update
    check_match 'nix run .#cmd.test' 'received' 'invalid output of service command'
  '';
}
