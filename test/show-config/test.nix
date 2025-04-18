{
  project = true;

  source = ''
  error_exact ${./config-ghci}
  preproc_error sub_store_hash
  step_run show-config ghci
  '';
}
