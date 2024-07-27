{...}:
{
  test = builtins.toFile "service-test" ''
    cd ./root
    flake_update
    check_match 'nix run .#cmd.test 2>&1' 'received'

    check 'nix run .#cmd.disabled-global' 'done' "invalid output of command 'disabled-global' – might have started VM"

    check 'nix run .#cmd.disabled-local' 'done' "invalid output of command 'disabled-local' – might have started VM"
  '';
}
