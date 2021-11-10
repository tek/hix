# About

A set of tools for developing on a Haskell project with Nix build.
Provides out-of-the-box setup for package overrides, `ghcid`, `haskell-language-server`, [thax], and `cabal upload`.

**Warning** This is still under construction, subject to breaking changes, unstable, and very specific to the author's
workflow.

# Basic usage

The simplest possible flake looks like this:

```nix
{
  description = "Spaceship";
  inputs.hix.url = github:tek/hix;
  outputs = { hix, ... }: hix.flake { base = ./.; packages = { spaceship = ./.; }; };
}
```

This will configure a single Cabal library at the root of the project, to be built with:

```
nix build .#spaceship
```

Running `main` from `app/Main.hs` in the package at `.` in a `ghcid` session can be done with:

```
nix run .#ghcid-test -- . Main main app
```

The effective `outputs` set looks like this:

```nix
{
  apps.x86_64-linux = {
    candidates = { ... }; # Run `cabal upload`
    docs = { ... }; # Run `cabal upload -d`
    ghcid-test = { ... }; # Run a function in `ghcid`
    hls = { ... }; # Run HLS
    hpack = { ... }; # Run `hpack`
    hpack-verbose = { ... }; # Run `hpack`
    release = { ... }; # Run `cabal upload --publish`
    tags { ... }; # Run `hasktags`
  };
  checks.x86_64-linux = {
    spaceship = { ... }; # Points to `packages.spaceship`
  };
  defaultApp.x86_64-linux = { ... }; # Points to `apps.ghcid-test`
  defaultPackage.x86_64-linux = { ... }; # Points to `packages.spaceship`
  devShell.x86_64-linux = { ... }; # shell derivation for `nix develop`
  legacyPackages.x86_64-linux = { ... }; # access to internals
  overrides = { ... }; # All configured GHC package overrides for use in dependent projects
  packages.x86_64-linux = {
    spaceship = { ... }; # `cabal2nix` derivation building the main package
  };
}
```

# Configuration

The function `hix.flake` combines multiple steps:

* `hix.haskell` creates a `nixpkgs` overlay with Cabal overrides for local packages and dependencies.
* `hix.tools` provides helpers for `ghcid`, HLS, `cabal upload`, `ctags` and `hpack`.
* `hix.flakeOutputs` assembles an `outputs.<system>` set according to flake standards.
* `hix.compatChecks` creates several additional copies of the GHC overlay for different versions.
* `hix.systems` iterates over the target systems (default is `["x86_64-linux"]`).

These functions share some parameters, so they are listed independently.

## Basics

|Name|Default|Description|
|---|---|---|
|`system`||Passed to `nixpkgs`, usually provided by `flake-utils`, which is called by `hix.systems`.|
|`base`||Path to the project root, should be specified as `./.`.|
|`packages`||Local Cabal [packages](#packages).|
|`main`|`packages.<singleton>`|The package used for `defaultPackage`. Defaults only if `packages` has one entry.|
|`compiler`|`"ghc8107"`|The attribute name of the GHC package set to use for development.|
|`overrides`|`{}`|[Dependency Overrides](#dependency-overrides).|
|`cabal2nixOptions`|`""`|Passed to `callCabal2nix` for local packages.|
|`profiling`|`true`|Whether to enable library profiling for dependencies.|
|`nixpkgs`|`inputs.nixpkgs`|`nixpkgs` used for development. `inputs.nixpkgs` refers to `hix`'s flake inputs, which can also be overridden with: `inputs.hix.inputs.nixpkgs.url = github:nixos/nixpkgs`|
|`nixpkgsFunc`|`import nixpkgs`|Function variant of the previous parameter. The default imports the specified `nixpkgs` argument.|
|`overlays`|`[]`|Additional overlays passed verbatim to `nixpkgs`.|
|`compat`|`true`|Create flake checks for other GHC versions.|
|`compatVersions`|`["901" "8107" "884"]`|GHC versions for which compat checks should be created.|

## Packages

The `packages` parameter is a set mapping project names to file system paths.
The simplest configuration, for a project with one Cabal file at the root, is:

```nix
{
  packages = {
    spaceship = ./.;
  };
}
```

For multiple packages:

```nix
{
  packages = {
    spaceship-core = ./packages/core;
    spaceship-api = ./packages/api;
  };
}
```

This configuration is used by `hix.haskell` to create `cabal2nix` derivations for the packages, by the `ghcid`
helpers to configure the include paths, and by the `cabal upload` scripts.

## GHC Compatibility Checks

If the `compat` argument is `true`, the flake will have additional outputs named like `compat-901-spaceship-core`.
These derivations don't share the same overrides as the main (`dev`) project.
This allows testing the project with the default packages from the hackage snapshot that nixpkgs uses for this version.
Each of these versions can have their own overrides, as described in the next section.


# Dependency Overrides

The `overrides` parameter allows the project's dependencies to be customized.
Its canonical form is a set mapping GHC versions to a list of special override functions, with an extra attribute for
the development dependencies and one that is used for _all_ package sets:

```nix
{
  overrides = {
    all = ...;
    dev = ...;
    ghc901 = ...;
    ghc8107 = ...;
  };
}
```

If, instead of a set, a list of override functions, or a single function, is given, they are treated as if they had been
specified as `{ all = overrides; }`.

Override functions have similar semantics to regular nixpkgs extension functions (`self: super: {...}`), but they take
additional parameters and can create not only derivations, but also custom dependency specifications.
The general shape is:

```nix
{
  overrides = {
    ghc901 = { self, super, hsLib, jailbreak, ... }: {
      name1 = hsLib.doJailbreak super.name1;
      name2 = jailbreak;
    };
  };
}
```

The function's parameter is a set containing the usual `self` and `super` as well as several other tools, including
built-ins, like `nixpkgs.lib`, and a set of composable depspec combinators, like `jailbreak`.

Here the override for `name1` jailbreaks the package in the usual way, while `name2` uses the special combinator for the
same purpose.
Composing those combinators is simple:

```nix
{
  overrides = {
    ghc901 = { profiling, jailbreak, hackage, ... }: {
      aeson = profiling (jailbreak (hackage "2.0.0.0" "shaxxxxx"));
      http-client = profiling jailbreak;
    };
  };
}
```

In the first case, the `hackage` combinator _sets_ the derivation to the one using version `2.0.0.0` from Hackage (using
the attribute name as the package), while the second case uses `super.http-client`.
The combinators can be understood as creating a pipeline that is given the `super` derivation as the default, with each
stage able to change it.

## Built-in Depspec Combinators

All of these are in the attribute set passed to an override function.

|Name|Derivation|
|---|---|
|`hackage`|Takes a version and SHA hash, sets the derivation to be that version pulled directly from Hackage.|
|`source.root`|Creates a derivation by running `cabal2nix` on a directory.|
|`source.sub`|Like `source.root`, but takes an additional subdirectory.|
|`source.package`|Like `source.sub`, but prepends `packages/` to the subdirectory.|
|`drv`|Sets a verbatim derivation.|
|`keep`|Sets the derivation to `null`, effectively falling back to `super`.|

|Name|Transformation|
|---|---|
|`unbreak`|Allow packages marked as `broken`.|
|`jailbreak`|Disable Cabal dependency bounds.|
|`configure`|Add a Cabal configure flag.|
|`configures`|Add multiple Cabal configure flags. |
|`override`|Pass a function to `overrideCabal`.|
|`minimal`|Disable Haddock, benchmarks and tests, and unbreak.|
|`profiling`|Force profiling.|

|Name|Option|
|---|---|
|`option`| Takes a key and an arbitrary value. Used to set options for derivation combinators.|
|`noHpack`| Sets an option with key `cabal2nix` to `--no-hpack`, which will be read by `source.*` and passed to  `cabal2nix`.|

## Creating Depspec Combinators

When evaluating the depspec, it is passed this state:

```nix
{
  drv = null;
  transform = id;
  options = {};
}
```

If `drv` is `null` in the result, it is replaced with `super.package`.
Then, the `transform` function is applied to it, which is composed from all the combinators like `jailbreak`.
`options` is an arbitrary set that can be used to modify other combinators.

A depspec combinator can be created with:

```nix
{
  withHaddock =
    hix.util.spec.create ({ self, super, final, prev, pkg, hsLib, lib }: {
      transform = drv: hsLib.doHaddock (prev.transform drv);
    });
}
```

`self` and `super` reference the regular Haskell package sets, while `final` and `prev` reference the depspec.
`self` and `final` point to the state after all overrides have been applied, while `super` and `prev` contain the state
that the previous combinator produced.
`pkg` contains the package name, while `hsLib` and `lib` are `nixpkgs.haskell.lib` and `nixpkgs.lib`.

There is a shorter way to construct this combinator:

```nix
{
  withHaddock =
    hix.util.spec.transform ({ hsLib, ... }: hsLib.doHaddock);
}
```

## Transitive Overrides

Overrides can be inherited from dependency flakes:

```nix
{
  inputs.dep1.url = github:me/dep1;

  outputs = { hix, dep1, ... }:
  hix.flake {
    base = ./.;
    paackages = ...;
    overrides = ...;
    deps = [dep1];
  };
}
```

The overrides defined in the flakes given in the `deps` argument will be folded into the current project's overrides,
with local overrides having higher precedence.
**Note** that this may lead to unexpected results if the dependencies don't use the same nixpkgs version.

# Tools

## `hpack`

These commands run `hpack` in each directory in `packages` (the first variant suppresses output):

```
nix run .#hpack
nix run .#hpack-verbose
```

It is possible to store the config files in a separate directory, configured by the `hpackDir` attribute to `flake`
(defaulting to `ops/hpack`).
If the file `${hpackDir}/<project-name>.yaml` exists, it will be copied to the project directory and removed after
running `hpack`.

Additionally, a shared directory, for use in `hpack` files with the `<<: !include shared/file.yaml` directive, may be
configured with the `hpackShared` parameter (defaulting to `shared`).
If the directory `${hpackDir}/${hpackShared}` exists, it will be linked to the project directory as well.

## `devShell` and `ghcid`

The project's local packages with all their dependencies are made available in the `devShell`, which can be entered with
`nix develop`.
In there, `cabal` commands work as expected.
Additionally, `ghcid` may be run with the proper configuration so that it watches all source files.

`ghcid` and `ghci` have several configuration options:

|Name|Default|Description|
|---|---|---|
|`ghci.basicArgs`|`["-Werror" "-Wall" "-Wredundant-constraints" "-Wunused-type-patterns" "-Widentities"]`|Passed directly to `ghci`.|
|`ghci.extraArgs`|`[]`|Passed directly to `ghci`.|
|`ghci.options_ghc`|`null`|If non-null, passed to `ghci` as `-optF`.|
|`ghcid.commands`|`_: {}`|A function taking `pkgs` and `ghc`, producing an attrset of attrsets. Each of those sets configure a [command](#commands).|
|`ghcid.prelude`|`true`|Whether to work around some issues with custom `Prelude`s.|
|`ghcid.shellConfig`|`_: {}`|Extra configuration for all `ghcid` apps, like extra search paths.|
|`ghcid.testConfig`|`_: _: {}`|Extra configuration for the test command.|

## Commands

The `ghcid.commands` attrset is translated into flake apps that run a haskell function in `ghcid`:

```nix
{
  outputs = { hix, ... }:
  hix.flake {
    ghcid.commands = { pkgs, ghc }: {
      dev-api = {
        script = ''
          :load Spaceship.Api.Dev
          :import Spaceship.Api.Dev (runDevApi)
        '';
        test = "runDevApi";
        config.env.DEV_PORT = "8000";
      };
    };
  };
}
```

```
nix run .#dev-api
```

An entry in that set has the following protocol:

|Attribute|Description|
|---|---|
|`script`|GHCi commands to load before running the test|
|`test`|Expression that should be evaluated|
|`config.env`|Environment variables passed to `mkDerivation`|
|`config.extraHaskellPackages`|Haskell packages to add to the environment|
|`config.buildInputs`|System packages to add to the environment|
|`config.extraSearch`|Additional GHCi search paths|
|`config.extraRestarts`|Additional paths that trigger a `ghcid` restart|
|`config.preCommand`|Shell command that should be executed before GHCi (on every reload)|
|`config.preStartCommand`|Shell command that should be executed before `ghcid` (once)|
|`config.exitCommand`|Shell command that should be executed after `ghcid` exits|
|`config.vm`|Configuration for a `qemu` VM that is started before and stopped after the command runs|

The values in the global parameter `shellConfig` are prepended to all values in `config`.

### Tests

Individual functions can be run in `ghcid` using the `nix` app `ghcid-test`:

```
nix run .#ghcid-test -- packages/spaceship-api Main main test generic
```

The parameters are as follows:

1. Path to the Cabal package that contains the function. Working directory is set to this path, (`.`,
  `packages/spaceship-api`).
2. Name of the module that contains the function (`Spaceship.Test.LaunchTest`).
3. Name of the function (`test_launch`).
4. Cabal component directory (`lib`, `app`, `test`) in the package, passed to `ghci` as `-i` (module search path).
5. Runner for the function.

This can be combined nicely with tools like [vim-test].

Runners are taken from the parameters `ghci.testScripts` and `ghci.testRunners` and some built-in ones:

|Name|Type|
|---|---|
|`hedgehog-property`|`Hedgehog.Property`|
|`hedgehog-unit`|`Hedgehog.TestT IO ()`|
|`tasty-tree`|`Tasty.TestTree`|
|`generic`|`IO ()`|

`testScripts` is an attrset mapping the runner name to a function `(moduleName :: String) -> (ghciScript :: String)`,
which should load the modules necessary to run the test:

```nix
{
  tasty-tree = module: ''
    :load ${module}
    import ${module}
    import Test.Tasty (defaultMain)
  '';
}
```

`testRunners` is an attrset mapping the runner name to a function
`(functionName :: String) -> (haskellExpression :: String)`:

```nix
{
  tasty-tree = name: "defaultMain ${name}";
}
```

The global option `testConfig` is used for the `config` parameters as described in [Commands](#commands).
It is called with the basic project as first arg and the executed test as second arg.

For example, setting the environment variable `BROWSER` to the path to chromium for tests:

```nix
{
  hix.flake {
    ...
    testConfig = { pkgs, ... }: _: {
      env.BROWSER = "${pkgs.ungoogled-chromium}/bin/chromium";
    };
  };
}
```

### Virtual Machines

If the command config attribute `vm` is given, a `qemu` VM is run for the command, for example to provide a database for
tests.
The config has the following protocol:

|Attribute|Default|Description|
|---|---|---|
|`create`|`null`|If given, override the entire built-in VM creation function|
|`type`|`null`|If given, use a special built-in VM config. Currently supported values: `["postgres"]`|
|`name`|`hix-vm`|Used for the temporary directory storing the image|
|`dir`|`/tmp/hix-vm/$USER/${name}`|Temporary directory storing the image|
|`basePort`|`10000`|Port from which the `ssh` port is calculated (`+ 22`)|
|`ports`|`[]`|Additional port forwardings in the format expected by `virtualisation.forwardPorts`|
|`conf`|`{}`|Additional config merged into the basic NixOS config for the VM|

If `type` is `postgres`, a database will be started in the VM with the following additional config:

|Attribute|Default|Description|
|---|---|---|
|`name`||Name of the database to be created, passed on to the basic VM creation function as well|
|`port`|`10000`|The port in the host system that is forwarded to PostgreSQL's port|
|`creds`|`{}`|Can contain `user` and `password`, both defaulting to `name`|
|`log`|`false`|Whether to enable logging|
|`conf`|`{}`|Additional config like for the basic VM, but merged after the PostgreSQL config|

If the `create` attribute is given, it should be a function that takes the config as an argument and returns an attrset
with the protocol:

|Attribute|Description|
|---|---|
|`main`|The VM derivation, as produced by `import "${nixpkgs}/nixos" { ... }`|
|`dir`|The directory containing the pidfile and image|
|`pidfile`|The pidfile path|

## `haskell-language-server`

HLS can be started with:

```
nix run .#hls
```

## `hasktags`

To generate `ctags` for all dependencies and project packages using [thax]:

```
nix run .#tags [<tags-file>]
```

`tags-file` defaults to `.tags`.

## `cabal upload`

To upload package candidates, publish or upload docs to Hackage:

```
nix run .#candidates
nix run .#release
nix run .#docs
```

If the arg `versionFile` is given, the script will substitute the `version:` line in that `hpack` file after asking for
the next version.

[thax]: https://github.com/tek/thax
[vim-test]: https://github.com/vim-test/vim-test
