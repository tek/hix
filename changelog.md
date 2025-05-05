# Unreleased

* Change the OC `ghcOptions` to accept a list of options as well as a string.
* Add OC `ghcOption` that only accepts a single string.
* Default to `language = "Haskell2010"` to avoid eager evaluation of environment configs.
* Add OC `noshared` that disables creation of shared libraries.
* Add OC `shared` that enables linking Haskell libraries dynamically in executables.
* Add special treatment for the override named `__all`, applying to the result of `mkDerivation`, and therefore all
  packages.
* Add the attribute `exe` to derivations in `build.*.executables`, containing the full path to the executable in the
  store.

# 0.9.0

* Add an experimental app, `maint`, that uploads Hackage revisions for previous tags when bounds can be bumped.
* Add `package.<package>.expose.target`, controlling whether a package is included in the default targets of envs.
* Add the option `envs.<env>.localOverrides` to control whether local packages are included in an env's GHC overrides,
  independently of whether they're exposed.
* Add an env, `hix-build-tools`, to configure packages used internally; like `cabal-install` and `hpack`.
* Add the option `envs.<env>.inheritOverrides` to control whether to include overrides from dependency flakes.
* Allow specifying non-local packages in `envs.<env>.packages`.
* Add the option `envs.<env>.globalOverrides` to control whether to include the global overrides.
* Add the app `release-source` for uploading source tarballs without docs to Hackage.
* Allow command scripts to be specified as paths as well as strings.
* Add `commands.<command>.buildInputs` to configure packages in `$PATH` for commands.
* Add `output.expose.static` to control whether packages outputs include `musl`-linked executables.
* Remove `nodoc` from the OC `force`, add `force'` as an alias of the old behavior.
* Add OC `modify` as a less unwieldy alias of `transformDrv`.
* Automatically compute overrides for the solver package set when managing deps.
* Use dependency information to assemble the GHCi search path.
* Allow mixed CLI options when running commands (including `ghci[d]`) with env selection enabled.

# 0.8.0

* Add a reusable workflow for managed bounds updates.
* Parse Nix json output messages to diagnose problems in managed bounds apps; add overrides for installed packages that
  fail with a bounds error printed by Cabal.
* Replace `envs.<env>.derivations` by per-output-category flags in `packages.<package>.expose` to control which packages
  are included in outputs.
* Add `envs.<env>.expose`, analogous to package exposition config.
* Add `dev` to the scoped envs to allow access to excluded derivations.
* Add override combinators for toggling Cabal flags.
* Remove packages with env name prefix (`ghc96-hix`) in favor of scoped derivations in `legacyPackages` (`ghc96.hix`).
* Expose intermediate and final build outputs as nested sets of environments and packages via module arguments `build`
  and `outputs`.
* Expose some computed attributes via the module argument `project` that were previously read-only module options.
* Add an override combinator for fetching from a different Hackage server.
* Add an override combinator for specifying the Hackage revision.
* Add an override combinator for specifying extra GHC options.
* Add an override combinator, `force`, that combines `unbreak`, `jailbreak`, `nodocs`, `nobench` and `notest`.
* Add an override combinator for disabling a dependency (and interpret `null` like that as well).
* Move some non-package outputs in `legacyPackages`, like `ghc` and `config`, to a subset with attribute name `project`.
* Move all nested outputs in `apps` and `packages` to `legacyPackages` to adapt to stricter validation in Nix 2.19.
* Expose `haskell.lib.compose` to override functions as `hsLibC`.
* Rename OC `cabalOverrides` to `cabal2nixOverrides`.

# 0.7.0: Hix Unbound: Majors Apart

* Fetch `thax` from a static URL, removing the flake input.
* Fix environment not being set when using `direnv`.
* Add an app, `bump`, that updates dependency versions and tests the build before writing new bounds and overrides to
  a file that's incorporated into the build.
* Add an app, `lower`, that  determines the lowest dependency versions with which the project builds.
* Add the option `envs.<name>.hoogle` to pass `withHoogle` to `ghcWithPackages`.
* Change the semantics of the option `profiled` to enable executable profiling.
  The old behavior has been moved to the option `libraryProfiling`.
* Add an environment `profiled` whose executables, with profiling enabled, are exposed as flake outputs like
  `.#hix.profiled` and `.#env.profiled.hix`.
* Add a new attribute to package outputs, `musl`, that uses the natively built toolchain available in `pkgsMusl`, as an
  alternative to the existing `cross.musl64`.

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
