{...}: {
  test = builtins.toFile "lower-test" ''
    cd ./root
    flake_update

    nix run .#lower.init.main -- --root $PWD || fail 'lower.init failed'

    check_diff ${./state-init.nix} 'ops/managed.nix' 'ops/managed.nix is wrong after init'

    nix run .#lower.optimize.main -- --root $PWD || fail 'lower.optimize failed'

    check_diff ${./state-optimize.nix} 'ops/managed.nix' 'ops/managed.nix is wrong after optimize'
  '';
}
