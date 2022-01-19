{ pkgs, keep ? false, ... }:
let
  modules = import ./modules/test.nix { inherit pkgs; };
  ghcid = import ./ghcid/test.nix { inherit pkgs; };
in {
  main = pkgs.writeScript "hix-tests" ''
  set -e
  hix_dir=$PWD
  tmpdir=/tmp/hix-test-temp
  fail()
  {
    echo -e ">>> Test '$current' failed!"
    echo -e ">>> $*"
    exit 1
  }
  ${if keep then "" else ''trap "rm -rf $tmpdir" EXIT''}

  prepare()
  {
    current="$1"
    testdir="$tmpdir/$1"
    cp -r "$hix_dir/test/$1" "$testdir"
    cd "$testdir"
    sed -i "s#HIX#$hix_dir#" */flake.nix
    sed -i "s#BASE#$testdir#" */flake.nix
    echo ">>> Running test '$current'..."
  }

  rm -rf $tmpdir
  mkdir -p $tmpdir

  prepare 'modules'
  ${if false then "" else modules.test}

  prepare 'ghcid'
  ${if false then "" else ghcid.test}

  echo '>>> All tests succeeded.'
  '';
}
