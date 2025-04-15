{
  project = true;

  source = ''
  error_exact ${./config-ghci}
  step_run show-config ghci
  '';
}
