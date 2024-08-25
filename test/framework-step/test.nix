{...}: {
  source = ''
  setopt local_options no_err_return

  TRAPEXIT()
  {
    if [[ -f _step_out ]]
    then
      cat _step_out
    fi
    unfunction false_positive false_negative require_success require_failure &>/dev/null
  }

  step_unexpected()
  {
    error_message "Step $step_number $1 unexpectedly"
    message "Output:"
    print '
  ---------- 8< ----------
    '
    cat _step_out
    print '
  ---------- 8< ----------
    '
    return 1
  }

  _after_assert()
  {
    if (( ''${_test_debug-0} == 1 ))
    then
      cat _step_out
    fi
    rm -f _step_out
    _test_debug=0
  }

  require_success()
  {
    trap _after_assert EXIT
    step $* &>_step_out
    if (( $? != 0 ))
    then
      step_unexpected "failed"
    fi
  }

  require_failure()
  {
    trap _after_assert EXIT
    step $* &>_step_out
    if (( $? == 0 ))
    then
      step_unexpected "succeeded"
    fi
  }

  require_output()
  {
    trap _after_assert EXIT
    step $@[2,$] &>_step_out
    diff <(print $1) _step_out
    if (( $? != 0 ))
    then
      error_message "Step $step_number output mismatch"
      return 1
    fi
  }

  debug()
  {
    _test_debug=1
  }

  local number=9

  output_exact 'equal: 9'
  require_success 'echo "equal: $number"' || return 1

  require_success 'echo output_exact is reset after a step' || return 1

  output_exact 'distinct: 8'
  require_failure 'echo distinct: $number' || return 1

  both_output()
  {
    print 'stdout message' >&1
    print 'stderr message' >&2
  }

  error_exact 'stderr message'
  require_success both_output || return 1

  error_exact 'wrong message'
  require_failure both_output || return 1

  error_match '.*err.*mes+age'
  require_success both_output || return 1

  error_match '.*err.mos+age'
  require_failure both_output || return 1

  output_rg -e 'stdout mes+a'
  require_success both_output || return 1

  error_rg -e 'stderr mes+a'
  require_success both_output || return 1

  local target1="[1m[35m>>>[0m[0m [31mStep 10 failed:[0m
  [1m[35m>>>[0m[0m   [1m[34mboth_output[0m[0m
  [1m[35m>>>[0m[0m Output doesn't match [34mrg[0m query:
  [1m[35m>>>[0m[0m   [1m[33m-U -e 'stdouts mes+a.*\\\\nstderr.*'[0m
  [1m[35m>>>[0m[0m Output:

  stdout message
  stderr message
  "

  combined_output
  output_rg -U -e 'stdouts mes+a.*\nstderr.*'
  require_output $target1 both_output || return 1

  combined_output
  output_rg -U -e 'stdout mes+a.*\nstderr.*'
  require_success both_output || return 1

  local target2='[1m[35m>>>[0m[0m [31mStep 12 failed:[0m
  [1m[35m>>>[0m[0m   [1m[34mboth_output[0m[0m
  [1m[35m>>>[0m[0m Cannot use [34mcombined_output[0m with [34merror_rg[0m'
  combined_output
  error_rg -e 'x'
  require_output $target2 both_output || return 1

  local target3="[1m[35m>>>[0m[0m [31mStep 13 failed:[0m
  [1m[35m>>>[0m[0m   [1m[34mboth_output[0m[0m
  [1m[35m>>>[0m[0m Output doesn't match regex:
  [1m[35m>>>[0m[0m   [1m[33mstdout mes\\\\n[0m
  [1m[35m>>>[0m[0m Output:

  stdout message

  [1m[35m>>>[0m[0m rg output:
  rg: the literal \"\\\\n\" is not allowed in a regex

  Consider enabling multiline mode with the --multiline flag (or -U for short).
  When multiline mode is enabled, new line characters can be matched."

  output_match 'stdout mes\n'
  require_output $target3 both_output || return 1

  preproc_output sub_store_hash
  output_exact '/nix/store/hash-pkg-1.0'
  require_success 'print /nix/store/aaa555zzzz999-pkg-1.0' || return 1

  bad_preproc()
  {
    print 'preproc error' >& 2
    return 1
  }

  local target4='[1m[35m>>>[0m[0m [31mStep 15 failed:[0m
  [1m[35m>>>[0m[0m   [1m[34mprint hello[0m[0m
  [1m[35m>>>[0m[0m Output preprocessor failed: [34mbad_preproc[0m
  preproc error'

  preproc_output bad_preproc
  require_output $target4 'print hello' || return 1
  unset bad_preproc
  '';
}
