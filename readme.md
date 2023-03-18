# About

Hix is a set of tools for developing a Haskell project with a Nix build definition.
It provides out-of-the-box setup for package overrides, `ghcid`, `haskell-language-server`, [thax], and `cabal upload`.

There is a dedicated page for the documentation of the library's [options].

# Basic usage

The main service provided by Hix is the construction of a set of [flake] outputs with useful functionality for
Haskell development.
A flake is an organizational unit for *nix* projects that usually corresponds to a repository.
It is configured in the file `flake.nix`, residing at the project's root directory, by defining a set of inputs
(dependencies that can be flakes or arbitrary source trees) and outputs (derivations, shells, overlays etc.).

The library's main entry point, `hix.lib.flake`, should be evaluated in the flake's `outputs` function.
Assuming the simplest possible project setup consisting of a single Cabal package named `spaceship` with an executable
module at `app/Main.hs`, the only necessary config option is the package name mapped to the source directory:

```nix
# flake.nix
{
  description = "Spaceship";
  inputs.hix.url = github:tek/hix;
  outputs = { hix, ... }: hix.lib.flake { packages.spaceship.src = ./.; };
}
```

This generates an output set that contains, among other things, the set `packages`, which is where `nix build` looks for
derivations, making it possible to build and run the `spaceship` package with the commands:

```
nix run .#gen-cabal
nix build .#spaceship
nix run .#spaceship
```

The first command is not necessary if you provide the Cabal file yourself.

Furthermore, the set `apps`, which is consulted by `nix run`, is populated with several tools, like a `ghcid` launcher.
This app allows the developer to run arbitrary functions in GHCi, instantly recompiling and re-executing on every
change.
For example, the function `main` from the module `Main` in the directory `app` in the package at the root directory
(`.`) can be executed with this command:

```
nix run .#ghcid -- . Main main app
```

`ghcid` is run in a Nix shell that is configured to provide the environment that the app needs to find the project
sources and all dependencies.
This shell environment can be entered interactively to run `cabal` with an appropriately configured package database:

```
nix develop
cabal v2-build spaceship
```

The full set of `outputs` looks roughly like this:

```nix
{
  apps.x86_64-linux = {
    candidates = { ... }; # Run `cabal upload`
    docs = { ... }; # Run `cabal upload -d`
    ghcid = { ... }; # Run a function in `ghcid`
    hls = { ... }; # Run HLS
    hpack = { ... }; # Run `hpack`
    hpack-quiet = { ... }; # Run `hpack`
    release = { ... }; # Run `cabal upload --publish`
    tags { ... }; # Run `hasktags`
  };
  checks.x86_64-linux = {
    spaceship = { ... }; # Points to `packages.spaceship`
  };
  defaultApp.x86_64-linux = { ... }; # Points to `apps.ghcid`
  defaultPackage.x86_64-linux = { ... }; # Points to `packages.spaceship`
  devShell.x86_64-linux = { ... }; # Shell derivation for `nix develop`
  legacyPackages.x86_64-linux = { ... }; # Access to internals
  overrides = { ... }; # All configured GHC package overrides for use in dependent projects
  packages.x86_64-linux = {
    spaceship = { ... }; # `cabal2nix` derivation building the main package
    min = { ... }; # Same as the main package, but skips some phases
  };
}
```

# Configuration

*Hix* uses the NixOS module system for configuration, allowing the user to override options that are deeply buried
within the library's logic with a type system and automatic documentation generation.
While there is a dedicated [options] page that lists all of them, here are a few basic ones:

|Name|Default|Description|
|---|---|---|
|`packages`||Local Cabal [packages](#packages).|
|`main`|`packages.<singleton>`|The package used for `defaultPackage`. Defaults only if `packages` has one entry.|
|`devGhc.compiler`|`"ghc925"`|The attribute name of the GHC package set to use for development.|
|`overrides`|`{}`|[Dependency Overrides](#dependency-overrides).|
|`compat.projects`|`{"943" = {}; "925" = {}; "902" = {};}`|GHC versions for which [compatibility checks](#ghc-compatibility-checks) should be created.|

While these options can be passed to `hix.lib.flake` as regular function arguments, the function actually treats its
argument as a NixOS module.
This allows complex configuration for more advanced projects with the ability to access the entire configuration:

```nix
{
  outputs = { hix, ... }:
  let
    module = { config, ... }: {
       packages = { spaceship.src = ./.; };
       output.systems = ["x86_64-linux" "aarch64-linux"];
       devGhc.compiler = if config.system == "aarch64-linux" then "ghc902" else "ghc925";
    };
  in hix.lib.flake module;
}
```

For even more elaborate setups, `hix.lib.flake` also accepts a *list* of modules.

## Packages

The `packages` option is a set mapping package names to their configuration.
The simplest configuration, for a project with one Cabal file at the root, is:

```nix
{
  packages = {
    spaceship.src = ./.;
  };
}
```

For multiple packages:

```nix
{
  packages = {
    spaceship-core.src = ./packages/core;
    spaceship-api.src = ./packages/api;
  };
}
```

This configuration is used to create `cabal2nix` derivations for the packages, by GHCi apps to configure the include
paths, and by the Hackage upload apps.

### Cabal configuration

The flake app `gen-cabal` transforms the configuration in `packages` to Cabal files in the respective source
directories.
[Many configuration options](options) are available to customize these files:

```nix
{config, lib, ...}: {

  cabal = {
    author = "Your Name";
    version = lib.mkDefault "0.5";
    ghc-options = ["-Wall"];
    default-extensions = ["OverloadedRecordDot" "DataKinds"];
    meta = {
      category = "Statistics";
    };
  };

  packages = {
    spaceship = {

      cabal = {
        version = "0.8";
        ghc-options = ["-Werror"];
        dependencies = ["transformers"];
      };

      library = {
        enable = true;
        dependencies = ["aeson"];
        ghc-options = ["-Wunused-imports"];
      };

      executable = {
        enable = true;
        source-dirs = "src";
      };

      tests.unit = {
        dependencies = ["hedgehog" "tasty"];
      };

    };
  };
}
```

Running the flake app will convert these options to JSON and pass them to [HPack], which generates a Cabal file:

```shell
nix run .#gen-cabal
```

The options in the top-level and package-level `cabal` modules cascade down into all packages and components,
respectively.

For example, the option `ghc-options = ["-Wall"]` will be used for all packages, and the same option (`[-Werror]`) in
the `spaceship` config will be used for all components in that package.
For list options, like `ghc-options` or `dependencies`, these will be merged, so that components will get all values
defined in the two `cabal` modules as well as those in the component config.

All options except for `src` have defaults, and specifying only the source dir will result in a cabal file that contains
a single executable using the `app` directory for Haskell sources.

The `meta` option is a freeform attribute set that will be passed verbatim to HPack at the package level.

Another freeform option, `cabal.cabal` (or just `cabal` at the component level), is used at the level it is defined and
does not cascade down.

### Minimal Derivations

Each package output has an additional output in the attribute `min` that has profiling disabled and skips haddock and
benchmarks.
In order to compile and test only, this derivation can be built with:

```shell
nix build .#spaceship-core.min
```

The main package's minimal derivation is additionally available as the package `min`:

```shell
nix build .#min
```

## GHC Compatibility Checks

If the `compat.enable` argument is `true`, the flake will have additional outputs named like
`compat-902-spaceship-core`.
These derivations don't share the same dependency overrides with the main (`dev`) project.
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
    ghc902 = ...;
    ghc925 = ...;
    ghc943 = ...;
  };
}
```

If, instead of a set, a list of override functions, or a single function, is given, they are treated as if they had been
specified as `{ all = overrides; dev = overrides; }`.
The explicit `dev` spec is used to allow this simpler variant to have precedence over explicit
[transitive](#transitive-overrides) `dev` overrides.

Override functions have similar semantics to regular nixpkgs extension functions (`self: super: {...}`), but they take
additional parameters and can create not only derivations, but also custom dependency specifications.
The general shape is:

```nix
{
  overrides = {
    ghc902 = { self, super, hsLib, jailbreak, ... }: {
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
    ghc902 = { profiling, jailbreak, hackage, ... }: {
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
|`notest`|Disable tests.|
|`minimal`|Disable Haddock, benchmarks and tests, and unbreak.|
|`fast`|Disable Haddock and profiling.|
|`profiling`|Force profiling.|
|`overrideAttrs`|Call the derivation's `overrideAttrs`.|
|`buildInputs`|Append to the derivation's `buildInputs`.|

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
`options` is an arbitrary set that can be used to influence the behaviour other combinators.

A depspec combinator can be created with:

```nix
{
  withHaddock =
    hix.lib.spec.create ({ self, super, final, prev, pkg, hsLib, lib }: {
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
    hix.lib.spec.transform ({ hsLib, ... }: hsLib.doHaddock);
}
```

Or, if none of the resources are needed:

```nix
{
  noCheck =
    hix.lib.spec.transform_ (drv: drv.overrideAttrs (_: { doCheck = false; }))
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
If a dependency's local packages should be included (and built directly from the flake's source), the alternative option
`depsFull` may be used.
**Note** that this may lead to unexpected results if the dependencies don't use the same nixpkgs version.

# Tools

## `devShell` and `ghcid`

The project's local packages with all their dependencies are made available in the `devShell`, which can be entered with
`nix develop`.
In there, `cabal` commands work as expected.
Additionally, `ghcid` may be run with the proper configuration so that it watches all source files.

`ghcid` and GHCi have several configuration options:

|Name|Default|Description|
|---|---|---|
|`ghci.args`|`[]`|Passed directly to GHCi.|
|`ghcid.commands`|`{}`|An attrset of submodules, each configuring a [command](#commands).|
|`ghcid.shellConfig`|`{}`|Extra configuration for all `ghcid` apps, like extra search paths.|
|`ghcid.testConfig`|`_: {}`|Extra configuration for the test command.|

### Commands

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
|`script`|GHCi commands to execute before running the test|
|`test`|Expression that should be evaluated|
|`shellConfig.env`|Environment variables passed to `mkDerivation`|
|`shellConfig.haskellPackages`|Haskell packages to add to the environment|
|`shellConfig.buildInputs`|System packages to add to the environment|
|`shellConfig.search`|Additional GHCi search paths|
|`shellConfig.restarts`|Additional paths that trigger a `ghcid` restart|
|`shellConfig.preCommand`|Shell command that should be executed before GHCi (on every reload)|
|`shellConfig.preStartCommand`|Shell command that should be executed before `ghcid` (once)|
|`shellConfig.exitCommand`|Shell command that should be executed after `ghcid` exits|
|`shellConfig.vm`|Configuration for a `qemu` VM that is started before and stopped after the command runs|

The values in the global parameter `shellConfig` are prepended to all values in `config`.

### Tests

Individual functions can be run in `ghcid` using the `nix` app `ghcid`:

```
nix run .#ghcid -- packages/spaceship-api Main main test generic
```

The parameters are as follows:

1. Path to the Cabal package that contains the function. Working directory is set to this path, (`.`,
  `packages/spaceship-api`).
2. Name of the module that contains the function (`Spaceship.Test.LaunchTest`).
3. Name of the function (`test_launch`).
4. Cabal component directory (`lib`, `app`, `test`) in the package, passed to GHCi as `-i` (module search path).
5. Runner for the function.

This can be combined nicely with tools like [vim-test].

Runners are taken from the options `ghci.scripts` and `ghci.runners` and some built-in ones:

|Name|Type|
|---|---|
|`hedgehog-property`|`Hedgehog.Property`|
|`hedgehog-unit`|`Hedgehog.TestT IO ()`|
|`tasty-tree`|`Tasty.TestTree`|
|`generic`|`IO ()`|

`ghci.scripts` is an attrset mapping the runner name to a function `(moduleName :: String) -> (ghciScript :: String)`,
which should load the modules necessary to run the test:

```nix
{
  ghci.scripts = {
    tasty-tree = module: ''
      :load ${module}
      import ${module}
      import Test.Tasty (defaultMain)
    '';
  };
}
```

`ghci.runners` is an attrset mapping the runner name to a function
`(functionName :: String) -> (haskellExpression :: String)`:

```nix
{
  ghci.runners = {
    tasty-tree = name: "defaultMain ${name}";
  };
}
```

The option `ghcid.testConfig` is used for the `config` parameters as described in [Commands](#commands).
It is called with an attrset containing the test parameters.

For example, setting the environment variable `BROWSER` to the path to chromium for integration tests:

```nix
{
hix.flake ({config, lib, ...}: {
  testConfig = {type, ...}:
    lib.optionalAttrs (type == "integration") {
      env.BROWSER = "${config.pkgs.ungoogled-chromium}/bin/chromium";
    };
  });
}
```

### Virtual Machines

If the command config option `vm.enable` is set to `true`, a `qemu` VM is run for the command, for example to provide
a database for tests.

The option `vm.conf` is passed directly to the import of `nixos`, allowing the user to run arbitrary services.
More configuration options are listed in the [options] documentation.

The option `vm.postgres.enable` adds a PostgreSQL server to the VM, with some additional configuration options in the
same module.

**Note**: If a virtual machine misbehaves, first try to delete the image at `/tmp/hix-vm` to make sure it's not an
effect of a nixpkgs update.

### Custom Preludes

If you're using a nonstandard `Prelude`, set these options (using `relude` as an example):

```nix
{
  cabal.prelude = {
    enable = true;
    package = {
      name = "relude";
      version = "^>= 1.2";
    };
    module = "Relude";
  };
}
```

This will produce HPack config containing something like:

```yaml
dependencies:
  - name: base
    version: '>= 4 && < 5'
    mixin:
      - hiding (Prelude)
  - name: relude
    version: "^>= 1.2"
    mixin:
      - (Relude as Prelude)
      - hiding (Relude)
```

For GHCi, using a custom prelude isn't quite as simple as adding Cabal mixins, since it does not read Cabal config
files.
It's possible to use `cabal repl` as the `ghcid` command to get around that, but automatic recompilation on file changes
doesn't work well with that method if you want to run tests, because Cabal doesn't support multiple components in the
repl unless you use the latest version.

In order to compensate for that, the `ghcid` app uses a GHC preprocessor that reads the Cabal file for each module and
inserts the Prelude, as well as extensions and `ghc-options`, into the header.

## Haskell Language Server

HLS can be started by running the `hls` app, using the same shell environment as `ghcid`:

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

If the option `versionFile` is set, the script will substitute the `version:` line in that *hpack* or `.cabal` file
after asking for the next version.
In the case of the *hpack* config being specified via the config option `hpack.packages`, this should be a `.nix` file
containing a string.
The options `hackage.versionFileExtract` and `hackage.versionFileUpdate` can be customized to allow for arbitrary other
formats.

The command line option `--version`/`-v` maybe be used to specify the version directly.
Furthermore, if one positional argument is given, only the package of that name will be uploaded.

For example, to publish `spaceship-api` at version `2.5.0.1`:

```
nix run .#release -- spaceship-api -v 2.5.0.1
```

[options]: https://tryp.io/hix/index.html
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
[HPack]: https://github.com/sol/hpack
[thax]: https://github.com/tek/thax
[vim-test]: https://github.com/vim-test/vim-test
