{...}: {
  test = builtins.toFile "app-rec-test" ''
    cd ./root
    flake_update

    check 'nix run .#a' 'a 2' "App 'a' failed"
    check 'nix run .#sub.b' 'b 2' "App 'b' failed"
    check 'nix run .#sub.c' 'c 1' "App 'c' failed"
    check 'nix run .#sub.c.d' 'd 2' "App 'd' failed"
    check_match_err 'nix run .#sub.c.e' "The option \`outputs.apps.sub.c.e' has conflicting definition values" "App 'e' failed"
  '';
}
