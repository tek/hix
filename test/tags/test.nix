{
  source = ''
  file_match 'hixNumber\s+\./Main.hs\s+3' .tags
  step_run tags
  '';
}
