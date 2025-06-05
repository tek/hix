{
  source = ''
  file_exact ${./candidates.yaml} .github/workflows/candidates.yaml
  step_run managed.gen.ga.releaseCandidates

  file_exact ${./publish.yaml} .github/workflows/publish.yaml
  step_run managed.gen.ga.releasePublish
  '';
}
