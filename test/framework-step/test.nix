{
  root = false;
  updateLock = false;

  source = ''
  setopt local_options no_err_return
  unset hix_test_show_stderr_failure

  TRAPEXIT()
  {
    if [[ -f _step_out ]]
    then
      hix_cat _step_out
    fi
    unfunction false_positive false_negative require_success require_failure &>/dev/null
  }

  step_unexpected()
  {
    error_message "Step $step_number $1 unexpectedly"
    message "Output:"
    hix_print '
  ---------- 8< ----------
    '
    hix_cat _step_out
    hix_print '
  ---------- 8< ----------
    '
    return 1
  }

  _after_assert()
  {
    if (( ''${_test_debug-0} == 1 ))
    then
      hix_cat _step_out
    fi
    rm -f _step_out
    _test_debug=0
  }

  require_success()
  {
    trap _after_assert EXIT
    step $* &> _step_out
    if (( $? != 0 ))
    then
      step_unexpected "failed"
    fi
  }

  require_failure()
  {
    trap _after_assert EXIT
    step $* &> _step_out
    if (( $? == 0 ))
    then
      step_unexpected "succeeded"
    fi
  }

  require_output()
  {
    trap _after_assert EXIT
    step $@[2,$] &> _step_out
    _hix_redirect diff <(print $1) _step_out
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
  require_success echo "equal: $number" || return 1

  local target_step2="[1m[35m>>>[0m[0m [31mStep [36m2[0m[31m failed:[0m
  [1m[35m>>>[0m[0m [31m  [1m[34mecho output_exact is reset after a step[0m[31m[1m[0m[31m[0m
  [1m[35m>>>[0m[0m Command produced output, but no check was defined.
  [1m[35m>>>[0m[0m Output:

  output_exact is reset after a step
  "

  require_output $target_step2 echo 'output_exact is reset after a step' || return 1

  output_exact 'distinct: 8'
  require_failure eval 'echo distinct: $number' || return 1

  both_output()
  {
    print 'stdout message' >&1
    print 'stderr message' >&2
  }

  output_ignore
  error_exact 'stderr message'
  require_success both_output || return 1

  output_ignore
  error_exact 'wrong message'
  require_failure both_output || return 1

  output_ignore
  error_match '.*err.*mes+age'
  require_success both_output || return 1

  output_ignore
  error_match '.*err.mos+age'
  require_failure both_output || return 1

  output_rg -e 'stdout mes+a'
  error_ignore
  require_success both_output || return 1

  error_rg -e 'stderr mes+a'
  output_ignore
  require_success both_output || return 1

  local target_step10="[1m[35m>>>[0m[0m [31mStep [36m10[0m[31m failed:[0m
  [1m[35m>>>[0m[0m [31m  [1m[34mboth_output[0m[31m[1m[0m[31m[0m
  [1m[35m>>>[0m[0m Output doesn't match [34mrg[0m query:
  [1m[35m>>>[0m[0m   [1m[33m-U -e 'stdouts mes+a.*\\\\nstderr.*'[0m
  [1m[35m>>>[0m[0m Output:

  stdout message
  stderr message
  "

  combined_output
  output_rg -U -e 'stdouts mes+a.*\nstderr.*'
  require_output $target_step10 both_output || return 1

  combined_output
  output_rg -U -e 'stdout mes+a.*\nstderr.*'
  require_success both_output || return 1

  local target_step12='[1m[35m>>>[0m[0m [31mStep [36m12[0m[31m failed:[0m
  [1m[35m>>>[0m[0m [31m  [1m[34mboth_output[0m[31m[1m[0m[31m[0m
  [1m[35m>>>[0m[0m Cannot use [33mcombined_output[0m with [33merror_rg[0m'
  combined_output
  output_ignore
  error_rg -e 'x'
  require_output $target_step12 both_output || return 1

  local target_step13="[1m[35m>>>[0m[0m [31mStep [36m13[0m[31m failed:[0m
  [1m[35m>>>[0m[0m [31m  [1m[34mboth_output[0m[31m[1m[0m[31m[0m
  [1m[35m>>>[0m[0m Output doesn't match regex:
  [1m[35m>>>[0m[0m   [1m[33mstdout mes\\\\n[0m
  [1m[35m>>>[0m[0m rg output:
  rg: the literal \"\\\\n\" is not allowed in a regex

  Consider enabling multiline mode with the --multiline flag (or -U for short).
  When multiline mode is enabled, new line characters can be matched.
  [1m[35m>>>[0m[0m Output:

  stdout message
  "

  output_match 'stdout mes\n'
  require_output $target_step13 both_output || return 1

  preproc_output sub_store_hash
  output_exact '/nix/store/hash-pkg-1.0'
  require_success print /nix/store/aaa555zzzz999-pkg-1.0 || return 1

  bad_preproc()
  {
    hix_print 'preproc error'
    return 1
  }

  local target_step15='[1m[35m>>>[0m[0m [31mStep [36m15[0m[31m failed:[0m
  [1m[35m>>>[0m[0m [31m  [1m[34mprint hello[0m[31m[1m[0m[31m[0m
  [1m[35m>>>[0m[0m Output preprocessor failed: [34mbad_preproc[0m
  preproc error'

  preproc_output bad_preproc
  require_output $target_step15 'print hello' || return 1
  unset bad_preproc

  bad_code()
  {
    return 95
  }

  exit_code 95
  require_success bad_code || return 1

  target_step17='[1m[35m>>>[0m[0m [31mStep [36m17[0m[31m failed:[0m
  [1m[35m>>>[0m[0m [31m  [1m[34mbad_code[0m[31m[1m[0m[31m[0m
  [1m[35m>>>[0m[0m Exit code [1m[31m95[0m[1m[0m, expected [32m15[0m
  [1m[35m>>>[0m[0m Output is empty.
  [1m[35m>>>[0m[0m stderr is empty.'

  exit_code 15
  require_output $target_step17 bad_code || return 1

  target_step18='[1m[35m>>>[0m[0m [31mStep [36m18[0m[31m failed:[0m
  [1m[35m>>>[0m[0m [31m  [1m[34mbad_code[0m[31m[1m[0m[31m[0m
  [1m[35m>>>[0m[0m Exit code [1m[31m95[0m[1m[0m
  [1m[35m>>>[0m[0m Output is empty.
  [1m[35m>>>[0m[0m stderr is empty.'

  require_output $target_step18 bad_code || return 1

  unset bad_code

  create_files()
  {
    print 'content1' > file1
    print 'content2' > file2
  }

  file_exact 'content1' 'file1'
  file_exact 'content2' 'file2'
  require_success create_files || return 1

  rm -f file1 file2

  target_step20='[1m[35m>>>[0m[0m [31mStep [36m20[0m[31m failed:[0m
  [1m[35m>>>[0m[0m [31m  [1m[34mcreate_files[0m[31m[1m[0m[31m[0m
  [1m[35m>>>[0m[0m Content of [34mfile1[0m does not match expectation:

  1c1
  < content2
  ---
  > content1

  < [32mexpected[0m[28m
  ---
  > [31mactual[0m[28m'

  file_exact 'content2' 'file1'
  require_output $target_step20 create_files || return 1
  '';
}
