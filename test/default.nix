{ pkgs, keep ? false, ... }:
let
  basic = import ./basic/test.nix { inherit pkgs; };
in {
  main = pkgs.writeScript "hix-tests" ''
  set -e
  hix_dir=$PWD
  tmpdir=/tmp/hix-test-temp
  fail()
  {
    echo -e ">>> Test failed!"
    echo -e ">>> $*"
    exit 1
  }
  ${if keep then "" else ''trap "rm -rf $tmpdir" EXIT''}

  rm -rf $tmpdir
  mkdir -p $tmpdir

  testdir="$tmpdir/basic"
  cp -r $hix_dir/test/basic $testdir
  cd $testdir
  ${basic.test}

  echo '>>> All tests succeeded.'
  '';
}
