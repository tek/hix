{...}: {
  test = builtins.toFile "app-rec-test" ''
    cd ./root
    flake_update

    check 'nix --quiet run .#a' 'a 2' "App 'a' failed"
    check 'nix --quiet run .#b.c' 'c 2' "App 'c' failed"
    check 'nix --quiet run .#b.d' 'd 1' "App 'd' failed"
    check 'nix --quiet run .#b.d.e' 'e 2' "App 'e' failed"
    check_match_err 'nix --quiet run .#b.d.f' "The option \`outputs.apps.b.d.f' has conflicting definition values" "App 'f' failed"

    dummy_target='[1m[35m>>>[0m[0m This app cannot be run, it is a namespace node with contents:
     [33m*[0m [34m.#outputs.apps.b.g.h[0m
     [33m*[0m [34m.#outputs.apps.b.g.i[0m'
    check_err 'nix --quiet run .#b.g' "$dummy_target" "App 'g' failed"
  '';
}
