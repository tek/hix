{

  source = ''
  step_build

  output_exact '"9.10.1"'
  step_eval env.dev.pkgs.haskell.compiler.hix.version

  output_exact 'false'
  step_eval env.dev.pkgs.haskell.compiler.hix.enableShared
  '';

}
