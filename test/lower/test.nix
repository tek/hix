{...}: {
  test = builtins.toFile "lower-test" ''
    cd ./root
    flake_update
    git init --quiet
    git add .
    git commit -m "init" --quiet
    args=(--index-state 2024-01-01T00:00:00Z --root $PWD)

    target_out_init='{"failed":[],"failedNames":null,"modified":[{"package":"aeson","range":">=2.2.0.0","version":"2.2.0.0"},{"package":"base","range":">=4.15.1.0","version":"4.15.1.0"},{"package":"extra","range":">=1.7.7","version":"1.7.7"}],"modifiedNames":"aeson, base, extra","unmodified":[],"unmodifiedNames":null}'

    out_init=$(nix run .#lower.init.main -- --output=json $args)
    if [[ $? != 0 ]]
    then
      fail 'lower.init failed'
    fi

    check_diff ${./state-init.nix} 'ops/managed.nix' 'ops/managed.nix is wrong after init (state-init.nix)'
    check_eq "$out_init" "$target_out_init" 'init json is wrong'
    check 'git status --short --porcelain -- ops/managed.nix' 'A  ops/managed.nix' 'ops/managed.nix was not git-added'

    nix run .#lower.optimize.main -- $args || fail 'lower.optimize failed'

    check_diff ${./state-optimize.nix} 'ops/managed.nix' 'ops/managed.nix is wrong after optimize (state-optimize.nix)'
  '';
}
