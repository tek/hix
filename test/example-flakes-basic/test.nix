{lib, system, ...}: {
  source = lib.optionalString (system == "x86_64-linux") ''
  describe 'flakes-basic'
  output_exact 'Hello'
  step_run greet
  '';
}
