{ pkgs, ... }:
{
  main = pkgs.writeScript "hix-tests" ''
  set -e
  base=$PWD
  cd $base/test/root
  nix build
  output=$(result/bin/run)
  if [[ $output != 'success' ]]
  then
    echo "Test for 'root' failed. Output:"
    echo $output
    exit 1
  fi
  '';
}
