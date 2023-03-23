{ pkgs }:
{
  test = builtins.toFile "ghcid-test" ''
    cd ./root
    nix flake update

    search_path='-i$PWD/dep/src:$PWD/src:$PWD/dep/lib:$PWD/lib:extra-search:$PWD/root/integration'
    result=$(nix eval --raw .#ghcid.testConfig_searchPath)
    if [[ "$result" != "$search_path" ]]
    then
      fail "search path set from test config doesn't match:\n$result"
    fi

    script=':load Root.Lib
import Root.Lib
putStrLn string'
    result=$(nix eval --raw .#ghcid.script)
    if [[ "$result" != "$script" ]]
    then
      fail "ghci script doesn't match:\n$result"
    fi

    if ! nix eval --raw .#ghcid.mainScript | grep -q 'ensure-vm'
    then
      fail "'ensure-vm' is not called in the 'mainScript'."
    fi

    output=$(nix run .#ghci-test <<< :quit 2>&1)

    if [[ ! "$output" =~ 'Prelude works' ]]
    then
      fail "invalid output from ghci command:\n$output"
    fi
  '';
}
