{system, lib, ...}: {

  # Check that boot config evaluation handles different module specifications – in particular, paths to modules that
  # contain functions or plain attrs.
  source = lib.optionalString (system == "x86_64-linux") ''
  output_exact '[ "x86_64-linux" ]'
  step_eval project.config.systems

  output_exact '3'
  step_eval a

  output_exact '3'
  step_eval b

  output_exact '3'
  step_eval c

  preproc_error 'sub_store_hash | drop_end 1 | take_end 3'
  error_exact ${./error}
  exit_code 1
  step_eval project.config.name
  '';

}
