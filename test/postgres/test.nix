{
  source = ''
    output_exact ' ?column? 
    ----------
            1
    (1 row)'
    preproc_output 'drop_end 1 | take_end 4'
    error_match 'Shutting down VM'
    step_run cmd.db-test
  '';
}
