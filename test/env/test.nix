{
  source = ''
  # TODO this can be 1 when a solution for component env fallback is implemented
  describe 'Plain command with component'
  output_exact 2
  step_run cmd.number

  describe 'Plain command without component'
  output_exact 1
  step_run cmd.number-nocomp

  describe 'Command with component selection'
  output_exact 2
  step_run cmd.number -p root -c app

  describe 'Command with file selection'
  output_exact 2
  step_run cmd.number -f $PWD/app/Main.hs

  describe 'Command with env selection via flake app attr'
  output_exact 3
  step_run env.three.number-nocomp

  describe 'Run command'
  output_exact 'The Glorious Glasgow Haskell Compilation System, version 9.8.4'
  step_run cmd.run ghc --version
  '';
}
