{...}: {
  test = builtins.toFile "bump-test" ''
    cd ./root
    flake_update

    check 'nix eval .#checkNames' '[ "latest-main-local1" "latest-other-local2" "local1" "local2" ]' 'checks are wrong'

    batch_result=$(nix run .#bump -- --root $PWD --handlers test || fail 'bump batch failed')

    check_diff '${./state.nix}' 'ops/managed.nix' 'ops/managed.nix is wrong after batch run'
    check_diff '${./batch-result.json}' $batch_result 'batch result is wrong'
    msg=$(nix run .#cli -- managed commit-msg --file $batch_result)
    check_diff '${./commit-msg}' =(print $msg) 'commit message is wrong'

    nix run .#env.latest-main.bump -- --root $PWD --handlers test || fail 'bump env failed'

    check_diff '${./state.nix}' 'ops/managed.nix' 'ops/managed.nix is wrong after env run'

    check_diff '${./local1.cabal}' 'packages/local1/local1.cabal' 'cabal is wrong'
  '';
}
