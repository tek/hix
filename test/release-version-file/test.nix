# Test that version files are updated during release flow.
# Tests three package scenarios:
# - pkg-cabal: Cabal version file
# - pkg-nix: Nix version file
# - pkg-none: No version file (not updated)
{...}: {
  genCabal = true;
  git = true;

  source = ''
version1="0.2.0.0"
version2a="0.3.0.0"
version2b="0.3.0.1"

describe 'Commit generated cabal files'
output_ignore
step git add .
step git commit --quiet -m 'gen-cabal'

describe 'Initial global Nix version file'
output_match '"0.1.0.0"'
step cat version.nix

describe 'Initial Cabal version file (pkg-cabal package)'
output_match 'version: *0.1.0.0'
step cat packages/pkg-cabal/pkg-cabal.cabal

describe 'Initial Nix version file (pkg-nix package)'
output_match '"0.1.0.0"'
step cat packages/pkg-nix/version.nix

describe 'Run release with version update only'
output_ignore
error_ignore
step_run release --version $version1 --commit --merge

describe 'Check managed state updated'
output_match "version = \"$version1\""
step cat ops/managed.nix

describe 'Check global Nix version file updated'
output_match "\"$version1\""
step cat version.nix

describe 'Check Cabal version file updated (pkg-cabal)'
output_match "version: *$version1"
step cat packages/pkg-cabal/pkg-cabal.cabal

describe 'Check Nix version file updated (pkg-nix)'
output_match "\"$version1\""
step cat packages/pkg-nix/version.nix

describe 'Check commit message'
output_exact "Release $version1"
step git show --format=format:'%s%n' --no-patch

describe 'Run release with a mix of versions'
output_ignore
error_ignore
file_exact ${./managed-1.nix} ops/managed.nix
step_run release --version $version2a --package pkg-nix --package pkg-cabal-$version2b --package pkg-none --commit --merge

describe 'No shared version, so global versionFile stays unchanged'
output_match "\"$version1\""
step cat version.nix

describe 'Explicit --package version for pkg-cabal overrides --version'
output_match "version: *$version2b"
step cat packages/pkg-cabal/pkg-cabal.cabal

describe 'No version with --package for pkg-nix uses --version arg'
output_match "\"$version2a\""
step cat packages/pkg-nix/version.nix
'';

}
