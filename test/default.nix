{ pkgs, keep ? false, ... }:
{
  main = pkgs.writeScript "hix-tests" ''
  set -e
  base=$PWD
  tmpdir=/tmp/hix-test-temp
  success='true'

  rm -rf $tmpdir
  mkdir -p $tmpdir

  cp -r $base/test/root/* $tmpdir/
  cd $tmpdir
  sed -i "s#HIX#$base#" flake.nix
  nix flake update
  nix run .#hpack-verbose
  nix build
  output=$(result/bin/run)
  if [[ $output != 'success' ]]
  then
    echo "Test for 'root' failed. Output:"
    echo $output
    success='false'
  fi
  ${if keep then "" else "rm -rf $tmpdir"}
  if [[ $success == 'false' ]]
  then
    exit 1
  fi
  '';
}
