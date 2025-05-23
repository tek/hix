let
  args = "--root $PWD --handlers test --hackage hackage.haskell.org:indexState:2024-01-01T00:00:00Z";
in {
  source = ''
    mk_pkg()
    {
      local i=$1 name=$2 code=$3
      local target="$work_dir/root/''${name}''${i}"
      mkdir -p "$target/lib"
      cp $work_dir/multi-fail/multi-fail.cabal "$target/multi-fail''${i}.cabal"
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

    target1="[35m[1m>>>[0m Building targets in [33mlatest[0m with [36mmulti-fail2-0.1.0[0m...
    [35m[1m>>>[0m Build with [36mmulti-fail2-0.1.0[0m failed in [36mmulti-fail1[0m, [36mmulti-fail2[0m
    [35m[1m>>>[0m Building targets in [33mlatest[0m with [36mmulti-fail1-0.1.0[0m...
    [35m[1m>>>[0m Build with [36mmulti-fail1-0.1.0[0m failed in [36mmulti-fail1[0m, [36mmulti-fail2[0m

    [35m[1m>>>[0m [33mlatest[0m
    [35m[1m>>>[0m Couldn't find working latest versions for some deps after 1 iteration.
        📦 multi-fail1
        📦 multi-fail2"

    error_exact $target1
    step_run bump ${args}

    mk_fail 1 'multi-fail'
    mk_success 2 'multi-fail'

    target2="[35m[1m>>>[0m Building targets in [33mlatest[0m with [36mmulti-fail2-0.1.0[0m...
    [35m[1m>>>[0m Build with [36mmulti-fail2-0.1.0[0m failed in [36mmulti-fail1[0m, [36mmulti-fail2[0m
    [35m[1m>>>[0m Building targets in [33mlatest[0m with [36mmulti-fail1-0.1.0[0m...
    [35m[1m>>>[0m Build with [36mmulti-fail1-0.1.0[0m failed in [36mmulti-fail1[0m, [36mmulti-fail2[0m

    [35m[1m>>>[0m [33mlatest[0m
    [35m[1m>>>[0m Couldn't find working latest versions for some deps after 1 iteration.
        📦 multi-fail1
        📦 multi-fail2"

    error_exact $target2
    step_run bump ${args}
  '';
}
