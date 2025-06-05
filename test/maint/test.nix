{pkgs, testlib, ...}: let

  test =
  # TODO add a dependency between local packages and ensure that everything works out.
  # Probably best to add a third package so that one can be separate.
  #
  # TODO make --commit --revision into module options
  testlib.hackage.withServer {} ''
  run_release()
  {
    step_run release \
    --hackage local:location:$_hackage_url \
    --candidates=none \
    --publish=sources \
    --commit \
    --tag \
    --merge \
    "$@"
  }

  step git commit --quiet --amend -C HEAD
  step git tag local1-1.0.0
  step git tag local2-1.0.0

  step touch file1
  step git add .
  step git commit -m "file1" --quiet

  error_match 'local1.*1\.1\.0'
  run_release --package local1-1.1.0

  step git tag local2-1.1.0

  step touch file2
  step git add .
  step git commit -m "file1" --quiet

  error_match 'local2.*1\.2\.0'
  run_release --package local2-1.2.0

  step_run maint --quiet --commit --revision --hackage local:location:$_hackage_url --maint-handlers=test-maint --build-handlers=test-maint

  output_match 'microlens >=0\.4 && <0\.6'
  step xh get --follow --auth test:test $_hackage_url/package/local1-1.1.0/revision/1.cabal

  output_match 'semigroups >=0\.19 && <0\.21'
  step xh get --follow --auth test:test $_hackage_url/package/local2-1.2.0/revision/1.cabal
  '';

in test // {
  genCabal = true;
  git = true;
}
