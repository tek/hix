{
  root = false;
  source = ''

  # ─── Scenario 1: Transitive compiler extension ───
  # base (source.build, tag=base-tag) -> mid (tag=mid-tag) -> leaf (flavour=quick)
  # Tests that source.build config propagates through two levels of extends,
  # and that partial overrides at each level work correctly.

  describe 'transitive-extend: leaf inherits source from base'
  output_match 'version = "9.10.1"'
  step_nix eval 'path:./transitive-extend#leaf-source'

  describe 'transitive-extend: mid overrides tag'
  output_exact '"mid-tag"'
  step_nix eval 'path:./transitive-extend#mid-tag'

  describe 'transitive-extend: leaf inherits tag from mid'
  output_exact '"mid-tag"'
  step_nix eval 'path:./transitive-extend#leaf-tag'

  describe 'transitive-extend: leaf name is its own attrName'
  output_exact '"leaf"'
  step_nix eval 'path:./transitive-extend#leaf-name'

  describe 'transitive-extend: leaf overrides flavour to quick'
  output_exact '"quick"'
  step_nix eval 'path:./transitive-extend#leaf-flavour'

  describe 'transitive-extend: mid inherits flavour from base'
  output_exact '"quickest"'
  step_nix eval 'path:./transitive-extend#mid-flavour'

  # ─── Scenario 2: Nonexistent compiler source ───
  # envs.bad.compiler = "ghcXYZ" — not in compilers.* or nixpkgs.
  # Should produce a clear error about the nonexistent GHC.

  describe 'nonexistent-source: error for unknown GHC name'
  exit_code 1
  preproc_error nix_error
  error_match 'ghcXYZ.*there is no such attribute'
  step_nix eval 'path:./nonexistent-source#project.config.envs.bad.toolchain.version'

  # ─── Scenario 3: Compiler shortcut vs package-set extends ───
  # env has compiler = "ghc910" (exists in compilers.*) and
  # package-set.extends = "my-ps" (which uses ghc98).
  # envDefault (800) should beat extendsDefault (900), so ghc910 wins.

  describe 'shortcut-vs-extends: env compiler shortcut wins over inherited'
  output_match '"9\.10\.'
  step_nix eval 'path:./shortcut-vs-extends#version'

  # ─── Scenario 4: Hash warning config path ───
  # extending-build extends build-base (which has source.build without hash).
  # The hash warning should mention build-base, but it uses extending-build's name.
  # This is a known infelicity: the warning config path is misleading.

  describe 'hash-warning-path: warning mentions extending compiler name (known issue)'
  combined_output
  output_match 'extending-build'
  step_nix eval 'path:./hash-warning-path#version'

  # ─── Scenario 5: False positive duplicate compiler warning ───
  # envs.custom.compiler = "ghc9101" — valid in nixpkgs but not in compilers.*.
  # The env wraps it as {source = "ghc9101"}, triggering a spurious
  # "duplicate compiler" warning because the package-set also has a default source.
  # Despite the warning saying "the former will be ignored", the shortcut IS used.

  describe 'false-positive-warning: duplicate warning fires but shortcut is used'
  combined_output
  output_match 'specifies both'
  step_nix eval 'path:./false-positive-warning#version'

  describe 'false-positive-warning: shortcut is actually used (version is 9.10.1)'
  combined_output
  output_match '"9\.10\.1"'
  step_nix eval 'path:./false-positive-warning#version'

  # ─── Scenario 6: Inline compiler in package-set extending a named compiler ───
  # The env's package-set uses an inline compiler that extends a named compiler
  # and overrides the tag. Tests extensibleOption with extender.

  describe 'inline-compiler-extends: compiler name includes package-set as extender'
  output_match 'test-env'
  step_nix eval 'path:./inline-compiler-extends#compiler-name'

  describe 'inline-compiler-extends: inline compiler inherits source from named'
  output_exact '"ghc98"'
  step_nix eval 'path:./inline-compiler-extends#compiler-source'

  describe 'inline-compiler-extends: inline compiler overrides tag'
  output_exact '"overridden-tag"'
  step_nix eval 'path:./inline-compiler-extends#compiler-tag'

  # ─── Scenario 7: Compiler shortcut referencing existing named compiler ───
  # envs.test-env.compiler = "ghc98" — exists in compilers.* from ghcVersionCompilers.
  # Should work cleanly with no warnings (no stderr).

  describe 'shortcut-existing: compiler shortcut to existing compiler works'
  output_match '"9\.8\.' 
  step_nix eval 'path:./shortcut-existing#version'

  '';
}
