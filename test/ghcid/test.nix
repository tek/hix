{ pkgs }:
{
  test = ''
    cd ./root
    nix flake update

    search_path='-i$PWD/lib:$PWD/root/intergration'
    result=$(nix eval --raw .#ghcid.testConfig_searchPath)
    if [[ "$result" != "$search_path" ]]
    then
      fail "search path set from test config doesn't match:\n$result"
    fi

    script=':load Prelude
import Prelude
:set -XImplicitPrelude
:load Root.Lib
import Root.Lib'
    result=$(nix eval --raw .#ghcid.script)
    if [[ "$result" != "$script" ]]
    then
      fail "ghci script doesn't match:\n$result"
    fi

    if ! nix eval --raw .#ghcid.mainScript | grep -q 'ensure-vm'
    then
      fail "'ensure-vm' is not called in the 'mainScript'."
    fi
  '';
}
