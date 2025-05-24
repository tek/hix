{
  source = ''
  describe "Envs aren't evaluated when generating HPack config"
  output_ignore
  step_eval legacyPackages.x86_64-linux.project.config.hpack.internal.packages

  describe "Non-builtin envs aren't evaluated when generating derivations"
  output_ignore
  step_eval packages.x86_64-linux.root.version

  describe "Non-builtin envs aren't evaluated when building derivations"
  step_build
  '';
}
