{system, ...}:
{
  genCabal = true;

  source = ''
  step_for()
  {
    describe "IFD package with missing dep is not forced when evaluating outputs in $(blue $1)"
    output_ignore
    step_output_names ''${1}.${system}
  }

  step_for packages
  step_for legacyPackages
  step_for checks
  step_for apps
  step_for devShells
  '';
}
