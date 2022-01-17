{ pkgs, keep ? false, ... }:
{
  main = pkgs.writeScript "hix-tests" ''
  set -e
  hix_dir=$PWD
  tmpdir=/tmp/hix-test-temp
  success='true'

  rm -rf $tmpdir
  mkdir -p $tmpdir

  cp -r $hix_dir/test/{root,dep1,dep2} $tmpdir/
  cd $tmpdir
  cat dep1/flake.nix
  sed -i "s#HIX#$hix_dir#" */flake.nix
  sed -i "s#BASE#$tmpdir#" */flake.nix
  cat dep1/flake.nix
  cd ./root
  nix flake update
  nix run .#hpack-verbose
  # nix build
  # output=$(result/bin/run)
  # if [[ $output != 'success66' ]]
  # then
  #   echo ">>> Test for 'root' failed. Output:"
  #   echo $output
  #   success='false'
  # fi
  version=$(nix eval .#stm-chans-version.${pkgs.system})
  if [[ $version != '"2.0.0"' ]]
  then
    echo ">>> Test failed: stm-chans version override in 'root' doesn't supersede the one from 'dep1' (is $version)."
  fi
  ${if keep then "" else "rm -rf $tmpdir"}
  if [[ $success == 'false' ]]
  then
    exit 1
  fi
  echo '>>> All tests succeeded.'
  '';
}
