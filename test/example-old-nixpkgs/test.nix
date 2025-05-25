{
  source = ''
  describe 'old-nixpkgs 1'
  output_exact 'success'
  step_run ghc90.root
  '';
}
