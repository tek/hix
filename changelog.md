# Unreleased

* Switch off IFD (i.e. cabal2nix) by default, allowing multiple systems to be exposed while still allowing flake checks
  to work.
* Change directory to the package root when running GHCi.

# 0.4.0.0

* Add commands for bootstrapping new and existing projects.

# 0.1.0.0

* Major rewrite moving Cabal config into the `packages` option.
* Add environments as a generalization of the GHC module.
* Add commands as a generalization of the GHCid runner.
* Add a CLI tool that acts as a GHC preprocessor for injecting extensions and prelude when running GHCi.
