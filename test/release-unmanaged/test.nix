# Test release behavior with managed.enable = false.
# When managed is disabled but a versionFile is configured:
# - Release proceeds without --force-version (version file provides persistence)
# - The managed state file should not be written
# - Version files should still be updated
{...}: {
  genCabal = true;
  git = true;

  source = ''
version1="0.2.0.0"

describe 'Commit generated cabal files'
output_ignore
step git add .
step git commit --quiet -m 'gen-cabal'

describe 'Release succeeds without --force-version when versionFile is configured'
output_ignore
error_ignore
step_run release --version $version1 --commit --merge

describe 'Version file updated'
output_match "\"$version1\""
step cat version.nix

describe 'Managed state file not created'
exit_code 1
step test -f ops/managed.nix

describe 'Commit message correct'
output_exact "Release $version1"
step git show --format=format:'%s%n' --no-patch

describe 'Remove versionFile from flake config'
step sed -i '/release.versionFile/d' flake.nix

describe 'Commit versionFile removal'
output_ignore
step git commit --quiet -a -m 'gen-cabal'

describe 'Release fails without version persistence'
output_ignore
exit_code 1
error_match 'managed.enable = true'
step_run release --version 0.3.0.0 --commit --merge

describe 'Release succeeds with --force-version without version persistence'
error_match 'Release completed successfully'
step_run release --force-version --commit --merge
  '';

}
