# Unreleased

* Fetch `thax` from a static URL, removing the flake input.
* Fix environment not being set when using `direnv`.

# 0.6.9

* Add the option `haskellTools` globally and per-env, which allows specifying shell tools that should be made available
  from the env's GHC without overrides.
* Separate the effect of the option `profiling` from `env.<name>.localPackage`.
* Add flake apps that build AppImage distributions using [nix-appimage](https://github.com/ralismark/nix-appimage) as
  `.#appimage`, `.#<exename>.appimage`, `.#env.<envname>.<exename>.appimage`.
* Add hooks to release process.

# 0.6.7

* The option `hackage.add` allows changed files to be git-added, but not committed, during release when `hackage.commit`
  is `false`.
* Default to `main` package when none was specified for `ghci(d)` commands in multi-package projects.

# 0.6.6

* Expose environment packages as `legacyPackages.<system>.<env>.<package>` for each entry in `ghcVersions`.
* Expose local packages, `ghc` and `pkgs` for each env as `legacyPackages.<system>.env.<env>.{<package>,ghc,pkgs}`.

# 0.6.2

* When releasing, the word `0.6.2` can be automatically replaced by the new version in changelogs when
  `hackage.setChangelogVersion` is `true` (default off).

# 0.6.1

* Allow envs to be excluded from being exposed as `devShells` for specific systems.
* Add an app, `dep-versions`, that prints all components' direct dependencies and their actual version in an
  environment.

# 0.6.0

* Support multiple Cabal libraries in a single package.
* Add GHCi(d) CLI options `--ghci-options` and  `--ghcid-options`.
* Allow cabal2nix derivations in overrides to be pre-generated and stored in a file to avoid IFD.

# 0.5.5

* Per-package `versionFile` is now used for the `version` field in Cabal files.

# 0.5.2

* Custom prelude preprocessor no longer mistakes modules with the prefix `Prelude` for the real thing.

# 0.5.1

* Add the top-level option `buildInputs` that specifies non-Haskell deps for all packages.

# 0.5.0

* Switch off IFD (i.e. cabal2nix) by default, allowing multiple systems to be exposed while still allowing flake checks
  to work.
* Change directory to the package root when running GHCi.
* Add the option `package.<name>.override` that allows transforming a derivation with override combinators.
* Add the option `package.<name>.buildInputs` that specifies non-Haskell deps for a package.

# 0.4.0.0

* Add commands for bootstrapping new and existing projects.

# 0.1.0.0

* Major rewrite moving Cabal config into the `packages` option.
* Add environments as a generalization of the GHC module.
* Add commands as a generalization of the GHCid runner.
* Add a CLI tool that acts as a GHC preprocessor for injecting extensions and prelude when running GHCi.
