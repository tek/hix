{
  git = true;

  source = ''
  file_exact ${./state-init.nix} 'ops/managed.nix'
  step_run lower --init --root $PWD

  file_exact ${./state-optimize.nix} 'ops/managed.nix'
  step_run lower optimize --root $PWD
  '';
}
