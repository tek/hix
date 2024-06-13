{...}: let
  args = "--root $PWD --handlers test --index-state 2024-01-01T00:00:00Z";
in {
  test = builtins.toFile "managed-nom-test" ''
    mk_pkg()
    {
      local i=$1 name=$2 code=$3
      local target="$testdir/root/''${name}''${i}"
      mkdir -p "$target/lib"
      cp $testdir/multi-fail/multi-fail.cabal "$target/multi-fail''${i}.cabal"
      print "module MultiFail where
    $code
    " > "$target/lib/MultiFail.hs"
      sed -i s#NUM#$i#g $target/**/*(.)
    }
    mk_success()
    {
      mk_pkg $1 $2 ""
    }
    mk_fail()
    {
      mk_pkg $1 $2 "import Data.Aeson (blergh)"
    }

    mk_success 1 'multi-fail-solve'
    mk_success 2 'multi-fail-solve'
    mk_success 1 'multi-fail'
    mk_fail 2 'multi-fail'

    cd ./root
    flake_update

    target1="[35m[1m>>>[0m Building targets in [33mlatest[0m with multi-fail2-0.1.0...
    [35m[1m>>>[0m Build with multi-fail2-0.1.0 failed in [34mmulti-fail1[0m, [34mmulti-fail2[0m
    [35m[1m>>>[0m Building targets in [33mlatest[0m with multi-fail1-0.1.0...
    [35m[1m>>>[0m Build with multi-fail1-0.1.0 failed in [34mmulti-fail1[0m, [34mmulti-fail2[0m

    [35m[1m>>>[0m [33mlatest[0m
    [35m[1m>>>[0m Couldn't find working latest versions for some deps after 1 iteration.
        📦 multi-fail1
        📦 multi-fail2"

    check 'nix --quiet run .#bump -- ${args} 2>&1' "$target1" 'First run with failing package has wrong output'

    mk_fail 1 'multi-fail'
    mk_success 2 'multi-fail'

    target2="[35m[1m>>>[0m Building targets in [33mlatest[0m with multi-fail2-0.1.0...
    [35m[1m>>>[0m Build with multi-fail2-0.1.0 failed in [34mmulti-fail2[0m, [34mmulti-fail1[0m
    [35m[1m>>>[0m Building targets in [33mlatest[0m with multi-fail1-0.1.0...
    [35m[1m>>>[0m Build with multi-fail1-0.1.0 failed in [34mmulti-fail2[0m, [34mmulti-fail1[0m

    [35m[1m>>>[0m [33mlatest[0m
    [35m[1m>>>[0m Couldn't find working latest versions for some deps after 1 iteration.
        📦 multi-fail1
        📦 multi-fail2"

    check 'nix --quiet run .#bump -- ${args} 2>&1' "$target2" 'Second run with failing package has wrong output'
  '';
}
