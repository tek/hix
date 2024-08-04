{pkgs, testlib, ...}: let

  test =
  # TODO add a dependency between local packages and ensure that everything works out.
  # Probably best to add a third package so that one can be separate.
  #
  # TODO make --commit --revision into module options
  testlib.hackage.withServer ''
  step git commit --quiet --amend -C HEAD
  step git tag local1-1.0.0
  step git tag local2-1.0.0

  step touch file1
  step git add .
  step git commit -m "file1" --quiet

  # This uses the local $CABAL_CONFIG to upload to the test hackage.
  output_match 'Release local1 1.1.0'
  step_run release local1 -v 1.1.0

  step git tag local2-1.1.0

  step touch file2
  step git add .
  step git commit -m "file1" --quiet

  output_match 'Release local2 1.2.0'
  step_run release local2 -v 1.2.0

  step_run maint --quiet --commit --revision --hackage local:location:$_hackage_url --handlers=test-maint

  output_match 'extra >=1\.7 && <1\.9'
  step xh get --follow --auth test:test $_hackage_url/package/local1-1.1.0/revision/1.cabal

  output_match 'semigroups >=0\.19 && <0\.21'
  step xh get --follow --auth test:test $_hackage_url/package/local2-1.2.0/revision/1.cabal
  '';

in test // {
  genCabal = true;
  git = true;
  path = pkgs: [pkgs.xh];
}
