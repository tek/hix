{ pkgs }:
{
  test = builtins.toFile "postgres-test" ''
    cd ./root
    nix flake update
    output=$(nix run .#cmd.db-test)

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
