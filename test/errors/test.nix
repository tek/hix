{
  root = false;
  source = ''

  # --- Success cases ---

  output_match 'success'
  step_nix run "path:./single-pkg"

  output_match 'success'
  step_nix run "path:./multi-src#foo"

  # --- packages.nix errors ---

  exit_code 1
  preproc_error "nix_error | strip_indent 7"
  error_exact "\
  error: The package 'foo' does not define the option 'src' in 'flake.nix'.
  This option should point to the root directory for this package, for example:

    packages = {
      foo.src = ./packages/foo;
      api.src = ./.;
    };

  In projects with a single package, Hix uses the project root as the default for 'src', but this project defines multiple packages."
  step_nix eval "path:./no-src-multi#project.config.packages.foo.src"

  exit_code 1
  preproc_error "nix_error | strip_indent 7"
  error_exact "\
  error: The package 'foo' does not define the option 'src' in 'flake.nix'.
  This option should point to the root directory for this package, for example:

    packages = {
      foo.src = ./packages/foo;
      api.src = ./.;
    };

  In projects with a single package, Hix uses the project root as the default for 'src'.
  This requires setting either of the options 'base' or 'self':

    outputs = {self, hix}: hix {
      inherit self;
      base = ./.;
      packages.foo = {};
    };"
  step_nix eval "path:./no-src-no-base#project.config.packages.foo.src"

  exit_code 1
  preproc_error "nix_error | strip_indent 7"
  error_exact "\
  error: The option 'main' is set to 'nonexistent', but no such package is defined.
  The available packages are: foo"
  step_nix eval "path:./nonexistent-main#project.config.name"

  # --- project.nix errors ---

  exit_code 1
  preproc_error "nix_error | strip_indent 7"
  error_exact "\
  error: The project does not configure any packages. Try adding this to 'flake.nix':

    packages.app.src = ./.;

  Or specify the project root to let Hix define a default package:

    base = ./.;

  Or use 'self' instead of 'base':

    outputs = {self, hix}: hix {
      inherit self;
    };

  Due to the hermeticity of Nix flakes, the directory cannot be inferred."
  step_nix eval "path:./no-dirs#project.config.packages.app.src"

  exit_code 1
  preproc_error "nix_error | strip_indent 7"
  error_exact "\
  error: Could not determine the main package.
  This should only happen if all packages depend on each other cyclically.
  If that is not the case, please report a bug at: https://github.com/tek/hix/issues
  You can specify the main package explicitly:
  {
    main = \"app\";
  }"
  step_nix eval "path:./ambiguous-main#project.config.main"

  # --- compilers.nix errors ---

  exit_code 1
  preproc_error "nix_error | take 1 | strip_indent 7"
  error_exact "\
  error: The compiler of the package-set of env 'bad' is configured to use the nixpkgs GHC 'ghc999', but there is no such attribute in 'pkgs.haskell.packages'."
  step_nix eval "path:./bad-ghc-version#project.config.envs.bad.toolchain.version"

  # --- ghc-build.nix errors ---

  exit_code 1
  preproc_error "nix_error | strip_indent 7"
  error_exact "\
  error: The GHC configuration 'custom' enables a custom build, but does not specify 'build.version'.
  This is required even if the version isn't used to select the sources."
  step_nix eval "path:./ghc-build-no-version#project.config.envs.custom.toolchain.version"

  # --- hpack.nix errors ---

  exit_code 1
  preproc_error "nix_error | strip_indent 7"
  error_exact "\
  error: Package 'base' is configured as the custom prelude with version bounds '>=4 && <5' and is also listed in 'dependencies' with version bounds '>=4.15'.
  Please specify bounds in only one place."
  step_nix run "path:./prelude-conflict#gen-cabal"

  # --- cabal-drv.nix errors ---

  exit_code 1
  preproc_error "nix_error | strip_indent 7"
  error_exact "\
  error: The Cabal config for 'root' in the env 'dev' has a dependency on the nonexistent package 'nonexistent-package-xyz'."
  step_nix build "path:./nonexistent-dep#packages.x86_64-linux.root"

  # --- nixpkgs.nix errors ---

  exit_code 1
  preproc_error "nix_error | strip_indent 7"
  error_exact "\
  error: The nixpkgs configuration named 'bad' must specify either 'url' or 'rev'."
  step_nix eval "path:./nixpkgs-no-source#project.config.envs.bad.toolchain.version"

  '';
}

