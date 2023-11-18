{...}: {
  test = builtins.toFile "bump-test" ''
    cd ./root
    flake_update

    check 'nix eval .#checkNames' '[ "latest-root" "root" ]' 'checks are wrong'

    nix run .#bump -- --root $PWD --handlers test || fail 'bump root failed'

    check_diff '${./state.nix}' 'ops/managed.nix' 'ops/managed.nix is wrong after root'

    nix run .#env.latest.bump -- --root $PWD --handlers test || fail 'bump env failed'

    check_diff '${./state.nix}' 'ops/managed.nix' 'ops/managed.nix is wrong after env'

    check_diff '${./root.cabal}' 'root.cabal' 'cabal is wrong'
  '';
}
