# Unreleased

* Allow envs to be excluded from being exposed as `devShells` for specific systems.

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
