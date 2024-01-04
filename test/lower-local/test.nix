{...}: {
  test = builtins.toFile "lower-local-test" ''
    cd ./root
    flake_update
    git init --quiet
    git add .
    git commit -m "init" --quiet

    nix run .#lower -- --init --root $PWD || fail 'lower.init failed'

    check_diff ${./state-init.nix} 'ops/managed.nix' 'ops/managed.nix is wrong after init'

    nix run .#lower optimize -- --root $PWD || fail 'lower.optimize failed'

    check_diff ${./state-optimize.nix} 'ops/managed.nix' 'ops/managed.nix is wrong after optimize'
  '';
}
