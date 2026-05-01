# Test multiple release commands, resetting the git history after each release to avoid false positives.
{pkgs}: let

  hook = pkgs.writeScript "hix-hook" ''
  #!/usr/bin/env bash
  set -e
  release-hook-marker
  '';

in
''
describe 'Substitute hackage port in flake'
step sed -i "s/HACKAGE_PORT/''${hackage_port}/" flake.nix

step git commit -q -a -m 'Hackage port'

version0="0.1.0.0"
version1="0.2.0"
version2="0.3.0"
version3="1.0.0"

describe 'version0 release'
error_ignore
output_ignore
step_run release --version=keep --candidates --publish --commit --tag --merge

describe 'version0 cabal local1'
output_exact ./packages/local1/local1.cabal
step fetch_cabal local1-$version0

describe 'version0 cabal local2'
output_exact ./packages/local2/local2.cabal
step fetch_cabal local2-$version0

describe 'version0 docs local2'
output_match 'docstring for local2'
step fetch_hackage package/local2-''${version0}/docs/Local2.html

describe 'version0 commit message'
output_exact "Release $version0"
step git show --format=format:'%s%n' --no-patch

describe 'version0 tag'
output_exact $version0
step git tag -l --points-at=HEAD

describe 'version0 post-upload hook'
output_exact $version0
step cat new-version

git reset --quiet --hard @^

describe 'version1 release'
error_ignore
output_ignore
file_exact "$version1: feature 1" changelog
file_exact "# $version1" packages/local1/ChangeLog.md
step_run release --candidates --publish --commit --tag --merge --version $version1

describe 'version1 cabal local1'
output_match "version: +$version1"
step fetch_cabal local1-$version1

describe 'version1 commit message'
output_exact "Release $version1"
step git show --format=format:'%s%n' --no-patch

describe 'version1 tag'
output_exact $version1
step git tag -l --points-at=HEAD

git reset --quiet --hard @^

bump_manually()
{
  cat > ops/managed.nix <<EOF
{
  packages = {
    local1 = { version = "$version2"; };
    local2 = { version = "$version2"; };
  };
}
EOF
  git add .
  git commit -m "bump manually" --quiet
}

describe "Substitute version manually in $(color_path ops/managed.nix)"
# file_match "$version2" ops/managed.nix
step bump_manually

describe 'version2 release'
error_ignore
output_ignore
step_run release --version=keep --publish --commit --tag --merge

describe 'version2 cabal local1'
output_match "version: +$version2"
step fetch_cabal local1-$version2

describe 'version2 commit message'
output_exact "Release $version2"
step git show --format=format:'%s%n' --no-patch

describe 'version2 tag'
output_exact $version2
step git tag -l --points-at=HEAD

git reset --quiet --hard @^^

describe 'version3 release'
error_ignore
output_ignore
step_run release --publish --commit --tag --merge --version $version3 --package local1

describe 'version3 cabal local1'
output_match "version: +$version3"
step fetch_cabal local1-$version3

describe 'version3: local2 was not uploaded'
output_match 'Package not found: No such package version for local2'
error_ignore
allow_failure
step fetch_cabal local2-$version3

describe 'version3 commit message'
output_exact "Release local1-$version3"
step git show --format=format:'%s%n' --no-patch

describe 'version3 tag'
output_exact local1-$version3
step git tag -l --points-at=HEAD

# Don't reset here - version4 continues from version3's state
# This allows version4 (1.0.0 -> 1.1.0) to be a valid minor bump

version4="1.1.0"

describe 'version4 release with explicit package-version'
error_ignore
output_ignore
# No --force-version needed: 1.0.0 -> 1.1.0 is a valid minor bump
step_run release --publish --commit --tag --merge --package local1-$version4

describe 'version4 cabal local1'
output_match "version: +$version4"
step fetch_cabal local1-$version4

describe 'version4: local2 was not uploaded'
output_match 'Package not found: No such package version for local2'
error_ignore
allow_failure
step fetch_cabal local2-$version4

describe 'version4 commit message'
output_exact "Release local1-$version4"
step git show --format=format:'%s%n' --no-patch

describe 'version4 tag'
output_exact local1-$version4
step git tag -l --points-at=HEAD

git reset --quiet --hard @^

version5="1.2.0"

describe 'version5 release with --candidates only (no --publish)'
output_ignore
# Test that --candidates alone enables candidate uploads
# Output should show 'local1 version (candidate)' format
# Note: Candidates are uploaded to a separate namespace, not fetchable via normal package path
error_match 'local1.*1.2.0.*candidate'
step_run release --candidates --commit --merge --version $version5 --package local1 --force-version

git reset --quiet --hard @^

# --- Git hooks test ---
# Test that git hooks run when --git=global is passed.
# The pre-commit hook calls `release-hook-marker` from $PATH.
# With hermetic (default) git, hooks are disabled, so hook-result should not be created.
# With --git=global, hooks run and the script must be found in $PATH.

mkdir -p .git/hooks
cp ${hook} .git/hooks/pre-commit
chmod +x .git/hooks/pre-commit
print 'hook-not-executed' > hook-result

version6="1.3.0"

describe 'version6 release with hooks disabled'
error_ignore
output_ignore
file_exact "hook-not-executed" hook-result
step_run release --publish=none --commit --merge --version $version6 --package local1 --force-version

git reset --quiet --hard @^

describe 'version6 release with hooks enabled (--git=global)'
error_ignore
output_ignore
file_exact "hook-executed" hook-result
step_run release --publish=none --commit --merge --version $version6 --package local1 --force-version --git=global

rm -f hook-result .git/hooks/pre-commit
git reset --quiet --hard @^
''
