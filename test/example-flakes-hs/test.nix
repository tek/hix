{
  source = ''
  describe 'flakes-hs'
  output_exact '{"number":1313}'
  step_run greet 1313
  '';
}
