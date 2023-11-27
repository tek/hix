{...}: {
  test = builtins.toFile "lower-test" ''
    cd ./root
    flake_update
    git init --quiet
    git add .
    git commit -m "init" --quiet

    target_out_init='{"failed":[],"failedNames":null,"modified":[{"package":"aeson","range":">=2.0.2.0 && <2.3","version":"2.0.2.0"},{"package":"extra","range":">=1.7.7 && <1.8","version":"1.7.7"}],"modifiedNames":"aeson, extra","operation":"lower","unmodified":[],"unmodifiedNames":null}'

    out_init=$(nix run .#lower.init.main -- --output=json --root $PWD)
    if [[ $? != 0 ]]
    then
      fail 'lower.init failed'
    fi

    check_diff ${./state-init.nix} 'ops/managed.nix' 'ops/managed.nix is wrong after init'
    check_eq "$out_init" "$target_out_init" 'init json is wrong'
    check 'git status --short --porcelain -- ops/managed.nix' 'A  ops/managed.nix' 'ops/managed.nix was not git-added'

    nix run .#lower.optimize.main -- --root $PWD || fail 'lower.optimize failed'

    check_diff ${./state-optimize.nix} 'ops/managed.nix' 'ops/managed.nix is wrong after optimize'
  '';
}
