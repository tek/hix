{ pkgs, config, }:
with pkgs.lib;
let
  lib = pkgs.lib;
  global = config;

  options = import ./options.nix { inherit pkgs; };
  inherit (options) namePh;

  util = import ../with-config.nix { inherit config lib util; };

  mod-ghc = options.module "ghc" { inherit global util; };

  mod-cabal-options = options.module "cabal-options" { inherit global util; };

  compExcept = [["source-dirs"] ["name"]];
  compMultiExcept = [[namePh "source-dirs"] [namePh "name"]];

  packageExclude = [
    { type = "sub"; path = ["cabal-config"]; }
    { type = "sub"; path = ["library"]; except = compExcept; }
    { type = "sub"; path = ["executable"]; except = compExcept; }
    { type = "sub"; path = ["executables"]; except = compMultiExcept; }
    { type = "sub"; path = ["test"]; except = compExcept; }
    { type = "sub"; path = ["tests"]; except = compMultiExcept; }
    { type = "sub"; path = ["benchmark"]; except = compExcept; }
    { type = "sub"; path = ["benchmarks"]; except = compMultiExcept; }
  ];

  mod-package = options.moduleWithout packageExclude "package" { global = config; inherit util; };

  text = content: { type = "text"; inherit content; };
  opt = name: options: { type = "options"; content = { inherit name options; }; };

  exampleFile = path: builtins.readFile (../../examples + "/${path}");

  header = ''
  # Hix Manual {#book-hix-manual}
  ## Nix development tools for Haskell projects
  '';

  about = ''
  ## About {#about}

  Hix is a toolkit for Haskell development that uses [Nix](https://nixos.org/learn.html) to provide a unified,
  declarative interface for a range of build related tasks:

  - Reproducible environments and dependency overrides
  - Cabal file generation
  - Hackage upload
  - Rapid-recompilation testing with `ghcid`
  - Haskell Language Server
  - CTags generation
  - Virtual Machines for testing
  - Compatibility checks for multiple GHC versions

  The following two sections explain the basics of Cabal builds and Nix flakes.
  If you're already familiar with those, you can skip ahead to [](#packages), but the later sections will build upon the
  examples introduced here.

  ## Cabal builds {#cabal-builds}

  Cabal is the basic build tool for Haskell projects.
  It reads package configuration from a `.cabal` file and performs compilation and execution of tests and applications.
  A Cabal package consists of one or more *components* like libraries, test suites and executables.

  The example project for this tutorial is called `parser` and defines a library and an executable.
  The library has a single module named `Parser.hs` in the subdirectory `lib/` with the content:

  ```haskell
  ${exampleFile "doc-packages/lib/Parser.hs"}
  ```

  The function `parseNumber` converts the first command line argument into an integer and returns a JSON string that
  looks like `{number:5}`, or an error message if the argument is not a number.
  It uses the JSON library [aeson](https://hackage.haskell.org/package/aeson) and the GHC core library
  [bytestring](https://hackage.haskell.org/package/bytestring), which have to be specified as dependencies in the Cabal
  file like this:

  ```cabal
  library
    exposed-modules:
        Parser
    hs-source-dirs:
        lib
    build-depends:
        aeson ==2.0.*
      , base ==4.*
      , bytestring
  ```

  We need to configure the source directory `lib`, the module `Parser` as well as the dependencies, including the
  standard library `base`.

  The second file in this project is the executable's entry point, located at `app/Main.hs`:

  ```haskell
  ${exampleFile "doc-packages/app/Main.hs"}
  ```

  The `main` function calls `parseNumber` and prints the returned string to stdout.
  It has no dependencies except for `base` and the library component, therefore its Cabal section is:

  ```cabal
  executable parser
    main-is: Main.hs
    hs-source-dirs:
        app
    build-depends:
        base ==4.*
      , parser
  ```

  The only difference here is that we need to specify the module that contains the `main` function with the key
  `main-is`.

  When these two fragments are written to `parser.cabal` in the project root along with a bit of boilerplate, the Cabal
  CLI tool be used to compile and run the application by invoking it as `cabal build` and `cabal run`.

  In order for this to work, the developer has to ensure that GHC and Cabal are installed and use the right version and
  package set snapshot to be certain that the application is built the same way on different machines.
  Hix aims to reduce the overhead of this process to requiring only the presence of Nix, with the goal of replicating
  the development environment and all involved tools identically on any machine.
  While Cabal offers some mechanisms for this kind of reproducibility, Nix provides a unified and ergonomic interface to
  it on multiple levels of the build.

  ## Nix flakes {#nix-flakes}

  The build definition for a Nix project is declared in the file `flake.nix` with the following
  [protocol](https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html):

  ```nix
  {
    inputs = {...};
    outputs = inputs: {
      packages = {...};
      apps = {...};
      ...
    };
  }
  ```

  The input set declares dependencies on other repositories, which are downloaded and passed to the output function as
  source directories when a Nix command is executed.
  Depending on the command, Nix chooses one of the entries in the output set and builds the requested package.

  A basic example for an application that prints "Hello" could look like this:

  ```nix
  ${exampleFile "doc-flakes-basic/flake.nix"}
  ```

  The single input is the nixpkgs repository at a specific commit, which contains both the build definitions for tens of
  thousands of available packages and many tools for creating new packages.

  The single output is an app named `greet` declared only for the architecture `x86_64-linux`, which can be executed
  with:

  ```
  nix run .#greet
  ```

  where the `.` denotes the directory of the flake and the part after the `#` is the name of the output, which Nix tries
  to locate in different categories depending on the command – in the case of `run`, it starts looking in `apps` using
  the current system's architecture.

  To gain access to nixpkgs' functionality, we import the `default.nix` (implicitly) at the root of the repository,
  passing the identifier of the architecture we want to use as the `system` argument.
  The function `writeScriptBin` is a so-called "trivial builder", a function that produces a very simple package, like a
  single shell script.

  Under the hood, Nix wraps the builder in a data structure called
  [derivation](https://nixos.org/manual/nix/unstable/language/derivations.html) that serves as the universal protocol
  for the specification of dependencies, build steps and build outputs.
  When Nix is instructed to process a derivation, it follows its build steps and writes the resulting files to a
  directory in `/nix/store`.
  In this case, the text `echo Hello` is written to `/nix/store/bkz0kkv0hxhb5spcxw84aizcj5rm4qq9-greet/bin/greet`, where
  the hash is calculated from the text and other environmental parameters.
  When the value `drv` is interpolated into the string in the declaration of the app output, Nix builds the derivation
  and inserts the resulting store path (minus the `bin/greet`).

  Derivations are ubiquitous – when we build a Haskell application with Nix, it is represented as a derivation, just
  like all of its dependencies.
  For the Cabal project described in the previous section, we can create a derivation and an app with this flake:

  ```nix
  ${exampleFile "doc-flakes-hs/flake.nix"}
  ```

  Now the app can be executed with:

  ```
  $ nix run .#parser -- 63
  {"number":63}
  ```

  To create a derivation for the Haskell app, we select a package set for the GHC version 9.2.5 with
  `pkgs.haskell.packages.ghc925` and call the builder exposed by that set that wraps the tool
  [cabal2nix](https://github.com/NixOS/cabal2nix), which converts the Cabal file in the specified directory to a
  derivation.
  Cabal2nix reads the dependencies from the file and ensures that they are accessible during the build, in which Cabal
  is invoked like in the previous section, though using a lower-level interface.

  If this project were cloned on a different machine, like in a CI pipeline, the nixpkgs snapshot, and hence the entire
  build environment, would be identical to that on your development machine – not just because the Git revision is
  hardcoded in the flake input, but because Nix records the revision in the lockfile, `flake.lock`.
  If the revision were omitted, the latest commit at the time the project was first evaluated would be used, and updated
  only when explicitly requested by executing either `nix flake update` (to update _all_ inputs) or `nix flake lock
  --update-input nixpkgs`.

  For a trivial Cabal project like this, the build definition may not look complicated enough to elicit the desire for
  higher abstractions.
  However, when the requirements become more elaborate, the basic tools provided by nixpkgs may prove inadequate, and
  the following sections attempt to illustrate some of the circumstances that warrant a more complex toolkit.

  '';

  basicUsage = ''
  ## Basic usage {#basic-usage}

  The primary functionality provided by Hix is the construction of a set of [flake] outputs with useful utilities for
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
  nix run path:.#gen-cabal
  nix build path:.#spaceship
  nix run path:.#spaceship
  ```

  The first command is not necessary if you provide the Cabal file yourself, and `path:` is optional if you add all files
  to git before running (or don't use git).

  Additionally, the set `apps`, which is consulted by `nix run`, is populated with several tools, like a `ghcid` launcher.
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
  '';

  configuration = ''
  ## Basic {#config-basic}

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
  '';

  packages = ''
  ## Package definitions {#packages}

  Hix provides two fundamental services to combine Cabal and Nix:
  - Generating Cabal build files from configuration declared in Nix expressions.
  - Creating reproducible derivations from those files for different configurable environments that obtain dependencies
    from the Nix package set, which can be built and run via the flake interface.

  The package from the tutorial section, consisting of an executable in `app/` and a library in `lib/` with dependencies
  on the packages `aeson` and `bytestring`, can be declared in a flake like this:

  ```nix
  ${exampleFile "doc-packages/flake.nix"}
  ```

  In order to build the project, we first have to generate the Cabal file:

  ```
  $ nix run .#gen-cabal
  ```

  ::: {.note}
  Flake commands ignore files that are not under version control when operating in a git repository.
  If there are untracked files in your project, running `nix build .#gen-cabal` might fail, so either `git add`
  everything or run the command as `nix run path:.#gen-cabal`, which bypasses this mechanism.
  :::

  This command will create the file `parser.cabal` in the project directory, with the following content:

  ```cabal
  ${exampleFile "doc-packages/parser.cabal"}
  ```

  Using the package definition and the generated Cabal file, Hix creates flake outputs for building and running the
  application with `nix build` and `nix run`:

  ```
  $ nix run .#parser -- 63
  {"number":63}
  ```

  The generated flake outputs have roughly the following structure, analogous to the example in {#nix-flakes}:

  ```nix
  {
    outputs = let
      parser = callCabal2nix "parser" ./. {};
    in {
      packages.x86_64-linux.parser = parser;
      apps.x86_64-linux.parser = { type = "app"; program = "''${parser}/bin/parser"; };
    };
  }
  ```

  ### Cabal configuration {#cabal-conf}

  There are three levels of generality at which Cabal options can be specified:

  - Global options in the module `cabal` apply to all packages and components
  - Package-level options in the module `packages.<name>.cabal` apply to all components in a package
  - Component-level options in all component modules

  The structure looks like this:

  ```nix
  {
    cabal = {
      dependencies = ["global-dep"];
    };
    packages = {
      core = {
        cabal = {
          dependencies = ["core-dep"];
        };
        library = {
          dependencies = ["core-lib-dep"];
        };
        executables.run = {
          dependencies = ["core-exe-dep"];
        };
      };
      api = {
        cabal = {
          dependencies = ["api-dep"];
        };
        library = {
          dependencies = ["api-lib-dep"];
        };
        tests.unit = {
          dependencies = ["api-test-dep"];
        };
      };
    };
  }
  ```

  Each component gets `global-dep`, while all components in `core` get `core-dep`.
  Since `dependencies` is a list option, the values are merged, so that the `unit` component in `api` will have the
  dependencies `["global-dep" "api-dep" "api-test-dep"]`.

  #### Cabal options {#submodule-cabal}

  These are the Cabal options for packages and components that can be specified at any level.

  '';

  packageOptions = ''
  ### Package configuration {#package-conf}

  Packages may contain multiple components: an optional library and any number of executables, test suites or benchmarks
  (Cabal does support multiple libraries now, but Hix doesn't yet).

  Each of those can be specified as the single component of its type using the singular-name option, like `executable`,
  or as a value in the plural-name submodule, or both:

  ```nix
  {
    packages.api = {
      executable = { enable = true; };
      executables = {
        server = { enable = true; source-dirs = "api-server"; };
        client = {};
        debug = { enable = false; };
      };
    };
  }
  ```

  This configuration generates three Cabal executables:

  - The default `executable` requires the option `enable` to be set explicitly and uses the source directory `app`
    unless specified otherwise.
  - The option `server` configures a custom source directory
  - The option `client` uses the default source directory, which is the same as the attribute name.
    All components configured in the plural-name submodule are enabled by default, so this one is also generated.
  - The option `debug` sets `enable` to `false`, so it is omitted from the configuration.

  If no component was enabled, Hix defaults to enabling the default executable.

  #### Package options {#submodule-package}

  '';

  devshell = ''
  When building a derivation for our package, the environment with GHC, Cabal and dependencies is contained in the build
  process.
  The same environment can be loaded into an interactive shell for the manual execution of arbitrary commands:

  ```
  nix develop
  ```

  This command examines the derivation given as the flake output `devShells.default` and makes all of its inputs
  available.
  Hix creates a devshell that provides GHC with Cabal and the project's dependencies in its package database, as well as
  a range of other tools.

  Once this shell is running, we can execute Cabal manually, resulting in the same build that is executed by the
  derivation:

  ```
  cabal build parser
  ```

  Unlike the derivation, this will only recompile files that have changed when executed repeatedly.

  ### Haskell Language Server

  The devshell includes HLS in the path, making it possible to instruct your editor to execute
  `nix develop -c haskell-language-server-wrapper`.
  For convenience, this is also exposed as a flake app:

  ```
  nix run .#hls
  ```

  ### GHCi

  During development, it is often desirable to execute a test whenever some code was changed.
  A common aproach to this is to load  the test module in GHCi.
  This task can be automated by using [ghcid](https://github.com/ndmitchell/ghcid), a wrapper around GHCi that reloads a
  module and its changed dependencies and runs a function when a file change is detected.

  Hix offers an interface for running `ghcid`:

  ```
  nix run .#ghcid -- ./. Main main test generic
  ```

  The meaning of the arguments is:

  - The Cabal package at `./.` (root of the project)
  - Import the module `Main`
  - Run the function `main`
  - Module is in the subdirectory `test`
  - The test runner is `generic` (for simple `IO` functions)

  '';

  chapters = [
    {
      tag = "intro";
      heading = "Introduction";
      fragments = [
        (text about)
      ];
    }
    {
      tag = "hix-build";
      heading = "Declaring Hix builds";
      fragments = [
        (text packages)
        (opt "cabal" mod-cabal-options)
        (text packageOptions)
        (opt "package" mod-package)
      ];
    }
    {
      tag = "devshell";
      heading = "Development shells";
      fragments = [
        (text devshell)
      ];
    }
  ];

  # TODO remove revision
  man = import ./manual.nix { inherit pkgs util header chapters; revision = "default"; };

in {
  json = man.optionsJSON;
  pkg = man.manualHTML;
}
