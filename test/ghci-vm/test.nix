{...}:
{
  root = true;
  updateLock = true;
  genCabal = true;

  source = ''
  describe 'Run service for GHCi app with component env'
  output_match 'test-endpoint'
  error_match 'Shutting down VM'
  step_ghci ghci -p root -t main

  describe 'Run service for GHCi app with explicit env'
  output_match 'test-endpoint'
  error_match 'Shutting down VM'
  step_ghci env.hix-ghci-test.ghci -t main
  '';
}
