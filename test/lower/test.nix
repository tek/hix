{
  git = true;

  source = ''
    args=(--index-state 2024-01-01T00:00:00Z --root $PWD)

    output_exact '{"failed":[],"failedNames":null,"modified":[{"package":"aeson","range":">=2.2.0.0","version":"2.2.0.0"},{"package":"base","range":">=4.15.1.0","version":"4.15.1.0"},{"package":"extra","range":">=1.7.7","version":"1.7.7"}],"modifiedNames":"aeson, base, extra","unmodified":[],"unmodifiedNames":null}'
    file_exact ${./state-init.nix} 'ops/managed.nix'
    step_run lower.init.main --output=json $args

    output_exact 'A  ops/managed.nix'
    step git status --short --porcelain -- ops/managed.nix

    file_exact ${./state-optimize.nix} 'ops/managed.nix'
    step_run lower.optimize.main $args
  '';
}
