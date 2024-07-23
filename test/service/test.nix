{...}:
{
  test = builtins.toFile "service-test" ''
    cd ./root
    flake_update
    check_match 'nix run .#cmd.test 2>&1' 'received'

    check_note 'nix run .#cmd.disabled-global' 'done' 'Might have started VM'

    check_note 'nix run .#cmd.disabled-local' 'done' 'Might have started VM'
  '';
}
