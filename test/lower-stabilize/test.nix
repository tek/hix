{...}: {
  test = builtins.toFile "lower-stabilize-test" ''
    cd ./root
    flake_update

    nix build .#env.lower.root || fail 'Initial build failed'

    cp ../RootUpdate.hs lib/Root.hs

    nix run .#lower.stabilize -- --root $PWD

    check_diff ${./state.nix} 'ops/managed.nix' 'ops/managed.nix is wrong after stabilize'
  '';
}
