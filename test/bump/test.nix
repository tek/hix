{...}: {
  test = builtins.toFile "bump-test" ''
    cd ./root
    flake_update

    check 'nix eval .#checkNames' '[ "latest-main-local1" "latest-other-local2" "local1" "local2" ]' 'checks are wrong'

    nix run .#bump -- --root $PWD --handlers test --output=commit-msg > commit-msg-out || fail 'bump commit-msg failed'
    check_diff '${./state.nix}' 'ops/managed.nix' 'ops/managed.nix is wrong after batch run'
    check_diff '${./commit-msg}' $PWD/commit-msg-out 'commit message is wrong'

    rm ops/managed.nix
    export GITHUB_OUTPUT="$PWD/github-output-out"
    nix run .#bump -- --root $PWD --handlers test --output=ga-pr || fail 'bump ga-pr failed'
    sed -i 's#hix-managed/bump-.*#hix-managed/bump-0#' $GITHUB_OUTPUT
    check_diff '${./github-output}' $GITHUB_OUTPUT 'github actions pr output is wrong'

    nix run .#env.latest-main.bump -- --root $PWD --handlers test || fail 'bump env failed'
    check_diff '${./state.nix}' 'ops/managed.nix' 'ops/managed.nix is wrong after env run'
    check_diff '${./local1.cabal}' 'packages/local1/local1.cabal' 'cabal is wrong'
  '';
}
