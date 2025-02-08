{
  source = ''
    output_exact 'a 2'
    step_run a

    output_exact 'c 2'
    step_run b.c

    output_exact 'd 1'
    step_run b.d

    output_exact 'e 2'
    step_run b.d.e

    error_match "The option \`outputs.apps.b.d.f' has conflicting definition values"
    exit_code 1
    step_run b.d.f

    error_exact '[1m[35m>>>[0m[0m This app cannot be run, it is a namespace node with contents:
    [1m[35m>>>[0m[0m   [33m*[0m [34m.#outputs.apps.b.g.h[0m
    [1m[35m>>>[0m[0m   [33m*[0m [34m.#outputs.apps.b.g.i[0m'
    step_run b.g
  '';
}
