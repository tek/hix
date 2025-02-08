{
  root = true;
  updateLock = true;

  source = ''
  output_match 'success'
  step_ghci ghci -p root -t main
  '';
}
