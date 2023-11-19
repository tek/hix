{...}: {
  test = builtins.toFile "lower-test" ''
    cd ./root
    flake_update
    git init --quiet
    git add .
    git commit -m "init" --quiet

    nix run .#lower.init.main -- --root $PWD || fail 'lower.init failed'

    check_diff ${./state-init.nix} 'ops/managed.nix' 'ops/managed.nix is wrong after init'
    check 'git status --short --porcelain -- ops/managed.nix' 'A  ops/managed.nix' 'ops/managed.nix was not git-added'

    nix run .#lower.optimize.main -- --root $PWD || fail 'lower.optimize failed'

    check_diff ${./state-optimize.nix} 'ops/managed.nix' 'ops/managed.nix is wrong after optimize'
  '';
}
