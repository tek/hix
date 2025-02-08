{...}: {
  source = ''
  describe 'Initial build'
  step_build env.lower.root

  describe 'Introduce breaking import change'
  cp ../RootUpdate.hs lib/Root.hs

  describe 'Stabilize lower bounds'
  file_exact ${./state.nix} 'ops/managed.nix'
  step_run lower.stabilize --root $PWD
  '';
}
