{...}: {
  test = builtins.toFile "lower-local-test" ''
    cd ./root
    flake_update
    git init --quiet
    git add .
    git commit -m "init" --quiet

    setopt local_options no_err_return
    output=$(nix run .#lower.init -- --root $PWD 2>&1)
    if [[ $? == 0 ]]
    then
      fail 'Expected lower.init failed with localsInPackageDb, but it did not'
    fi
    setopt local_options err_return

    sed -i 's/localsInPackageDb = true/localsInPackageDb = false/' flake.nix

    nix run .#lower.init -- --root $PWD || fail 'lower.init failed'

    check_diff ${./state-init.nix} 'ops/managed.nix' 'ops/managed.nix is wrong after init'

    nix run .#lower.optimize -- --root $PWD || fail 'lower.optimize failed'

    check_diff ${./state-optimize.nix} 'ops/managed.nix' 'ops/managed.nix is wrong after optimize'
  '';
}
