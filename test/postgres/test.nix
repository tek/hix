{ pkgs }:
{
  test = builtins.toFile "postgres-test" ''
    cd ./root
    flake_update
    output=$(nix run .#cmd.db-test | tail -n5)

    target=' ?column? 
    ----------
            1
    (1 row)'

    if [[ $output != $target ]]
    then
      fail "invalid output of db test command:\n$output"
    fi
  '';
}
