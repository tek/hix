_step_failed()
{
  error_message "Step $(cyan $step_number) failed:"
  message_hang "$(bold $(color_shell_cmd $step_cmd))"
}

_step_validate_stream()
{
  if [[ -z $_step_stream || $_step_stream == 'initial' || $_step_stream == 'finished' ]]
  then
    error_message 'Test framework internal error:'
    message_hang "$(color_variable '$_step_stream') wasn't initialized properly. Value: '$(red "$_step_stream")'"
    return 1
  fi
}

_step_file_empty()
{
  local size=$(stat -c %s $1)
  if [[ $_step_consider_only_control_chars_unexpected_output ]]
  then
    (( $size == 0 ))
  else
    (( $size < 100000 )) && [[ $(ansifilter $1) =~ '^\s*$' ]]
  fi
}

_step_stream_desc_upper()
{
  if [[ $_step_stream == 'err' ]]
  then
    print 'stderr'
  elif [[ $_step_stream == 'out' ]]
  then
    print 'Output'
  else
    print "Unrecognized stream '$_step_stream'"
  fi
}

_step_show_output()
{
  local output=$1
  _step_validate_stream
  local desc=$(_step_stream_desc_upper)
  if _step_file_empty $output
  then
    message "$desc is empty."
  else
    message "$desc:"
    local lines=$(wc -l $output | cut -d' ' -f1)
    if (( $lines > 22 )) && [[ -z $hix_test_full_output ]]
    then
      message "Output is $lines lines long, showing last 20."
      message "Use $(color_shell_cmd 'export hix_test_full_output=1') to show everything."
      hix_print ''
      _hix_redirect tail -n 20 $output
    else
      hix_print ''
      hix_cat $output
    fi
    hix_print ''
  fi
}

_step_show_stdout()
{
  _step_stream='out'
  _step_show_output $step_stdout
  _step_finish_stream 0
}

_step_show_stderr()
{
  _step_stream='err'
  _step_show_output $step_stderr
  _step_finish_stream 0
}

_step_show_stderr_after()
{
  local result=$1
  if (( $_step_stderr_shown == 1 ))
  then
    return
  fi
  if [[ -n $hix_test_show_stderr ]] || (
    (( $result == 1 )) &&
    [[ -n $hix_test_show_stderr_failure ]]
  )
  then
    if _step_file_empty $step_stderr
    then
      message "stderr is empty."
    else
      message "stderr:"
      hix_cat $step_stderr
    fi
  fi
}

_step_start_validation()
{
  eval "_step_validated_$1=1"
}

_step_ensure_validated()
{
  local slug=$1 desc=$2
  local p_name="_step_processed_std$slug" flag_name="_step_validated_$slug"
  if ! _step_file_empty ${(P)p_name}
  then
    if [[ ${(P)flag_name} != 1 ]]
    then
      _step_failed
      message "Command produced $desc, but no check was defined."
      eval "_step_show_std$slug"
      return 1
    fi
  fi
}

_step_ensure_validated_out()
{
  _step_ensure_validated 'out' 'output'
}

_step_ensure_validated_err()
{
  _step_ensure_validated 'err' 'stderr'
}

_step_apply_preproc()
{
  local desc=$1 cmd=$2 out=$3 out_pp=$4
  trap "rm -f ${out}.err" EXIT
  eval "{ $cmd } < $out > $out_pp 2>${out}.err"
  if (( $? != 0 ))
  then
    _step_failed
    message "$desc preprocessor failed: $(blue $cmd)"
    hix_cat ${out}.err
    return 1
  fi
}

_step_apply_preprocs()
{
  local pp_out=${_step_preproc_out:-cat} pp_err=${_step_preproc_err:-cat}
  _step_processed_stdout="${step_stdout}.processed"
  _step_processed_stderr="${step_stderr}.processed"
  _step_apply_preproc 'Output' $pp_out $step_stdout $_step_processed_stdout &&
    _step_apply_preproc 'stderr' $pp_err $step_stderr $_step_processed_stderr
}

_step_check_exit_code()
{
  local msg="Exit code $(bold $(red $step_exit_code))"
  if (( ${_step_require_exit_code-0} != 0 ))
  then
    if (( $_step_require_exit_code != $step_exit_code ))
    then
      _step_failed
      message "$msg, expected $(green $_step_require_exit_code)"
      _step_show_stdout
      _step_show_stderr
      return 1
    fi
  elif (( ${_step_allow_failure-0} == 0 )) && (( $step_exit_code != 0 ))
  then
    _step_failed
    message $msg
    _step_show_stdout
    _step_show_stderr
    return 1
  fi
}

_step_check_combined()
{
  if (( ${_step_combined_output-0} == 1 ))
  then
    _step_failed
    message "Cannot use $(color_function combined_output) with $(color_function $1)"
    return 1
  fi
}

_step_diff_output()
{
  local actual_file=$1 expected=$2
  diff <(print $expected) $actual_file &> $step_diff
  if (( $? != 0 ))
  then
    _step_failed
    _step_validate_stream
    message "$(_step_stream_desc_upper) mismatch:"
    hix_print ''
    hix_cat $step_diff

    hix_print "
< $(green 'expected')
---
> $(red 'actual')"
    return 1
  fi
}

_step_check_exact_out()
{
  if  [[ -n $_step_exact_out ]]
  then
    _step_start_validation 'out'
    _step_diff_output $_step_processed_stdout $_step_exact_out
  fi
}

_step_check_exact_err()
{
  if [[ -n $_step_exact_err ]]
  then
    _step_start_validation 'err'
    _step_check_combined 'error_exact' && _step_diff_output $_step_processed_stderr $_step_exact_err
  fi
}

_step_check_rg_gen()
{
  local desc=$1 repr=$2 actual_file=$3 args=($@[4,$])
  rg $args $actual_file &>$step_match
  if (( $? != 0 ))
  then
    _step_failed
    _step_validate_stream
    message "$(_step_stream_desc_upper) doesn't match $desc:"
    message_part_hang "$bold_start$yellow_start"
    # TODO is this still necessary with the new sgr hack? Was the reason for this that the global zsh option interfered
    # with parsing? If so, we've now made the test case runner hermetic.
    _hix_redirect print -rn -- $repr
    _hix_redirect print -- "$reset_sgr"
    _step_show_output $actual_file
    if ! _step_file_empty $step_match
    then
      message 'rg output:'
      hix_cat $step_match
    fi
    return 1
  fi
}

_step_check_rg()
{
  local actual_file=$1
  local args=($@[2,$])
  local repr=(${(q-)args})
  _step_check_rg_gen "$(color_command rg) query" "$repr" $actual_file $args
}

_step_check_regex()
{
  local actual_file=$1 regex=$2
  _step_check_rg_gen 'regex' "$regex" $actual_file '-e' "$regex"
}

_step_check_rg_out()
{
  if [[ -n $_step_rg_out ]]
  then
    _step_start_validation 'out'
    _step_check_rg $_step_processed_stdout $_step_rg_out
  fi
}

_step_check_rg_err()
{
  if [[ -n $_step_rg_err ]]
  then
    _step_start_validation 'err'
    _step_check_combined 'error_rg' && _step_check_rg $_step_processed_stderr $_step_rg_err
  fi
}

_step_check_regex_out()
{
  if [[ -n $_step_regex_out ]]
  then
    _step_start_validation 'out'
    _step_check_regex $_step_processed_stdout $_step_regex_out
  fi
}

_step_check_regex_err()
{
  if [[ -n $_step_regex_err ]]
  then
    _step_start_validation 'err'
    _step_check_combined 'error_match' && _step_check_regex $_step_processed_stderr $_step_regex_err
  fi
}

_step_finish_stream()
{
  _step_stream='finished'
  return $1
}

_step_check_out()
{
  _step_stream='out'
  _step_check_exact_out &&
  _step_check_rg_out &&
  _step_check_regex_out &&
  _step_ensure_validated_out
  _step_finish_stream $?
}

_step_check_err()
{
  _step_stream='err'
  _step_check_exact_err &&
  _step_check_rg_err &&
  _step_check_regex_err &&
  _step_ensure_validated_err
  _step_finish_stream $?
}

step()
{
  setopt local_options no_err_return local_traps
  local _step_diff result _step_stderr_shown=0 _step_stream='initial'

  if [[ -z $step_number ]]
  then
    step_number=0
  fi
  (( step_number ++ ))

  step_dir="$test_base/step-${step_number}"
  step_stdout="$step_dir/stdout"
  step_stderr="$step_dir/stderr"
  step_diff="$step_dir/diff"
  step_match="$step_dir/match"
  step_cmd="$*"

  mkdir -p $step_dir
  touch $step_stdout
  touch $step_stderr

  if (( ${_step_combined_output-0} == 1 ))
  then
    eval "$step_cmd" &> $step_stdout
  else
    eval "$step_cmd" 1> $step_stdout 2> $step_stderr
  fi
  local step_exit_code=$?


  if _step_apply_preprocs &&
     _step_check_exit_code &&
     _step_check_out &&
     _step_check_err
  then
    result=0
  else
    result=1
  fi

  _step_show_stderr_after $result

  unset \
    _step_exact_out \
    _step_rg_out \
    _step_regex_out \
    _step_out_processed \
    _step_exact_err \
    _step_rg_err \
    _step_regex_err \
    _step_err_processed \
    _step_preproc_out \
    _step_preproc_err \
    _step_allow_failure \
    _step_require_exit_code \
    _step_combined_output \
    _step_validated_out \
    _step_validated_err
  return $result
}

step_nix()
{
  step "nix --show-trace -L $@"
}

step_run()
{
  step_nix "run .#$1 -- ${@[2,$]}"
}

step_develop()
{
  step_nix "develop -c $@"
}

output_exact()
{
  _step_exact_out=$*
}

error_exact()
{
  _step_exact_err=$*
}

output_rg()
{
  typeset -ga _step_rg_out
  _step_rg_out=($@)
}

error_rg()
{
  typeset -ga _step_rg_err
  _step_rg_err=($@)
}

output_match()
{
  _step_regex_out=$*
}

error_match()
{
  _step_regex_err=$*
}

preproc_output()
{
  _step_preproc_out=$*
}

preproc_error()
{
  _step_preproc_err=$*
}

allow_failure()
{
  _step_allow_failure=1
}

combined_output()
{
  _step_combined_output=1
}

sub_store_hash()
{
  sed -r 's#/nix/store/[a-z0-9]+#/nix/store/hash#g'
}

output_ignore()
{
  _step_start_validation 'out'
}

error_ignore()
{
  _step_start_validation 'err'
}

exit_code()
{
  _step_require_exit_code=$*
}
