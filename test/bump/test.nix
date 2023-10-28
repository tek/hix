{...}: {
  test = builtins.toFile "bump-test" ''
    cd ./root
    flake_update

    target='{
      deps = {
        root = {
          aeson = "^>=2.2";
          extra = ">=1.7 && <1.8";
          th-abstraction = "^>=0.5";
        };
      };
      overrides = {
        latest = {
          aeson = {
            version = "2.2.0.0";
            hash = "1rxbydr7mvchhlyz2111n70db90s1zd9h6miqbbqh2kyc2l0b3pd";
          };
          extra = {
            version = "1.7.14";
            hash = "0dvp4h3grnfkly6aj7j9ic62qgy4gzvn8c8s8d4ncx6kjglgwn3v";
          };
          th-abstraction = {
            version = "0.5.0.0";
            hash = "0dkilfrvk8zdn3gvyfv5zgjbwqhdf1yg90fk4byka0ib43kgkyvf";
          };
        };
      };
    }'

    nix run .#bump.root -- --root $PWD --handlers test >/dev/null || fail 'bump root failed'

    check 'cat ops/deps.nix' $target 'ops/deps.nix is wrong after root'

    nix run .#env.latest.bump -- --root $PWD --handlers test >/dev/null >/dev/null || fail 'bump env failed'

    check 'cat ops/deps.nix' $target 'ops/deps.nix is wrong after env'

    check_diff 'root.cabal' '${./root.cabal}' 'cabal is wrong'
  '';
}
