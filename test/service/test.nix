{
  source = ''
    describe 'Run a command with a VM service'
    output_match 'received'
    error_match 'Shutting down VM'
    step_run cmd.test

    describe "No VM when $(yellow 'service.*.enable = false')"
    output_exact 'done'
    step_run cmd.disabled-global

    describe "No VM when $(yellow 'envs.*.services.*.enable = false')"
    output_exact 'done'
    step_run cmd.disabled-local
  '';
}
