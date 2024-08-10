_step_failed()
{
  error_message "Step $step_number failed:"
  message_hang "$(bold $(blue $step_cmd))"
}

_step_output_desc_upper()
{
  if [[ $1 == $step_stdout ]]
  then
    print 'Output'
  else
    print 'stderr'
  fi
}

_step_file_empty()
{
  local size=$(stat -c %s $1)
  (( $size == 0 ))
}

_step_show_output()
{
  local output=$1
  local sort=$(_step_output_desc_upper $output)
  if _step_file_empty $output
  then
    message "$sort is empty."
  else
    message "$sort:"
    local lines=$(wc -l $output | cut -d' ' -f1)
    if (( $lines > 22 )) && [[ -z $hix_test_full_output ]]
    then
      message "Output is $lines lines long, showing last 20."
      message "Use $(blue 'export hix_test_full_output=1') to show everything."
      print ''
      tail -n 20 $output
    else
      print ''
      cat $output
    fi
    print ''
  fi
}

_step_show_stdout()
{
  _step_show_output $step_stdout
}

_step_show_stderr()
{
  _step_show_output $step_stderr
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
      cat $step_stderr
    fi
  fi
}

_step_check_exit_code()
{
  local msg="Exit code $(bold $(red step_exit_code))"
  if (( ${_step_require_exit_code-0} != 0 ))
  then
    if (( $_step_require_exit_code != ${step_exit_code-111} ))
    then
      _step_failed
      message "$msg, expected $(green ${_step_require_exit_code})"
      _step_show_stdout
      return 1
    fi
  elif (( ${_step_allow_failure-0} == 1 )) && (( ${step_exit_code-111} != 0 ))
  then
    _step_failed
    message $msg
    _step_show_stdout
    return 1
  fi
}

_step_check_combined()
{
  if (( ${_step_combined_output-0} == 1 ))
  then
    _step_failed
    message "Cannot use $(blue combined_output) with $(blue $1)"
    return 1
  fi
}

_step_diff_output()
{
  local actual_file=$1 expected=$2
  diff <(print $expected) $actual_file &>$step_diff
  if (( $? != 0 ))
  then
    _step_failed
    message 'Output mismatch:'
    print ''
    cat $step_diff

    print "
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
    _step_diff_output $step_stdout $_step_exact_out
  fi
}

_step_check_exact_err()
{
  if [[ -n $_step_exact_err ]]
  then
    _step_check_combined 'error_exact' && _step_diff_output $step_stderr $_step_exact_err
  fi
}

_step_check_rg_gen()
{
  local desc=$1 repr=$2 actual_file=$3 args=($@[4,$])
  rg $args $actual_file &>$step_match
  if (( $? != 0 ))
  then
    _step_failed
    message "$(_step_output_desc_upper $actual_file) doesn't match $desc:"
    message_part_hang "$bold_start$yellow_start"
    print -rn -- $repr
    print -- "$reset_sgr"
    _step_show_output $actual_file
    if ! _step_file_empty $step_match
    then
      message 'rg output:'
      cat $step_match
    fi
    return 1
  fi
}

_step_check_rg()
{
  local actual_file=$1
  local args=($@[2,$])
  local repr=(${(q-)args})
  _step_check_rg_gen "$(blue rg) query" "$repr" $actual_file $args
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
    _step_check_rg $step_stdout $_step_rg_out
  fi
}

_step_check_rg_err()
{
  if [[ -n $_step_rg_err ]]
  then
    _step_check_combined 'error_rg' && _step_check_rg $step_stderr $_step_rg_err
  fi
}

_step_check_regex_out()
{
  if [[ -n $_step_regex_out ]]
  then
    _step_check_regex $step_stdout $_step_regex_out
  fi
}

_step_check_regex_err()
{
  if [[ -n $_step_regex_err ]]
  then
    _step_check_combined 'error_match' && _step_check_regex $step_stderr $_step_regex_err
  fi
}

step()
{
  setopt local_options no_err_return
  local _step_diff result _step_stderr_shown=0
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
    eval "$step_cmd" &>$step_stdout
  else
    eval "$step_cmd" 1>$step_stdout 2>$step_stderr
  fi
  step_exit_code=$?

  if _step_check_exit_code &&
     _step_check_exact_out &&
     _step_check_exact_err &&
     _step_check_rg_out &&
     _step_check_rg_err &&
     _step_check_regex_out &&
     _step_check_regex_err
  then
    result=0
  else
    result=1
  fi

  _step_show_stderr_after $result

  unset _step_exact_out _step_exact_err _step_rg_out _step_rg_err _step_regex_out _step_regex_err _step_allow_failure
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

allow_failure()
{
  _step_allow_failure=1
}

combined_output()
{
  _step_combined_output=1
}
