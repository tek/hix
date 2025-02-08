{testlib, ...}:
{
  git = true;

  source = ''
  ${testlib.hackage.releaseFixtures}

  describe 'release-all-pristine-version run'
  preproc_output 'sub_store_hash | take 2'
  output_exact $pristine_target
  step_run release

  describe 'release-all-pristine-version commit message'
  output_exact "Release $pristine_version"
  step git show --format=format:'%s%n' --no-patch

  describe 'release-all-pristine-version tag'
  output_exact $pristine_version
  step git tag -l --points-at=HEAD

  describe 'release-all-pristine-version postUploadAll hook'
  output_exact '0.1.0.0'
  step cat new-version

  describe 'release-all-pristine-version preCommitAll hook'
  output_exact '1'
  step cat source

  git reset --quiet --hard @^

  describe 'release-all-version run'
  preproc_output 'sub_store_hash | take 2'
  output_exact $version_target
  file_exact "$version: feature 1" changelog
  file_exact "# $version" ChangeLog.md
  step_run release -v $version

  describe 'release-all-version commit message'
  output_exact "Release $version"
  step git show --format=format:'%s%n' --no-patch

  describe 'release-all-version tag'
  output_exact $version
  step git tag -l --points-at=HEAD

  bump_manually()
  {
    sed -i "s/^version:\(\s*\).*/version:\1$manual_version/" root.cabal
    git add .
    git commit -m "bump manually" --quiet
  }

  describe "Substitute version manually in $(color_path root.cabal)"
  step bump_manually

  describe 'release-all-version-manual run'
  preproc_output 'sub_store_hash | take 2'
  output_exact $manual_target
  step_run release

  describe 'release-all-version-manual commit message'
  output_exact "Release $manual_version"
  step git show --format=format:'%s%n' --no-patch

  describe 'release-all-version-manual tag'
  output_exact $manual_version
  step git tag -l --points-at=HEAD

  git reset --quiet --hard @^^^

  describe 'candidates-all run'
  preproc_output 'sub_store_hash'
  output_exact $candidates_target
  step_run candidates

  describe 'release-one run'
  preproc_output 'sub_store_hash | take 2'
  output_exact $version_target
  step_run release root -v $version

  describe 'release-one commit message'
  output_exact "Release root $version"
  step git show --format=format:'%s%n' --no-patch

  describe 'release-one tag'
  output_exact root-$version
  step git tag -l --points-at=HEAD

  git reset --quiet --hard @^

  describe 'docs-all run'
  preproc_output 'sub_store_hash'
  output_exact "$(doc_target true)"
  step_run docs

  describe 'docs-one run'
  preproc_output 'sub_store_hash'
  output_exact "$(doc_target true)"
  step_run docs root
  '';

}
