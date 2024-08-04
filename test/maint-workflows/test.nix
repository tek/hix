{
  source = ''
  file_exact ${./maint.yaml} .github/workflows/maint.yaml
  step_run managed.gen.ga.maint

  file_exact ${./revision.yaml} .github/workflows/revision.yaml
  step_run managed.gen.ga.revision
  '';
}
