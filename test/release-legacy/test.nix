{...}:
{
  git = true;

  source = ''
  describe 'Error message when running release without arguments'
  output_exact '
'
  step_run release

  '';

}
