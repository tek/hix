{
  source = ''
  describe 'env-selection 1'
  output_exact 2
  step_run cmd.number

  describe 'env-selection 2'
  output_exact 4
  step_run cmd.number -p root -c app 2

  describe 'env-selection 3'
  output_exact 4
  step_run cmd.number --env four
  '';
}
