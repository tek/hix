{hixUrl}: let

  exampleFile = path: builtins.readFile (../../examples + "/${path}");

in {

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
  - Rapid-recompilation testing with GHCid
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
  `pkgs.haskell.packages.ghc92` and call the builder exposed by that set that wraps the tool
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

  ### Generating flakes {#gen-flake}

  Rather than writing the boilerplate yourself, the Hix CLI application can generate it for you.

  The CLI command `new` will create a project skeleton with an executable and test suite in the current directory:

  ```
  nix run '${hixUrl}#new' -- --name 'project-name' --author 'Your Name'
  ```

  If you have an existing project with Cabal files in it, the `bootstrap` command will create a flake that configures
  the more basic components:

  ```
  nix run '${hixUrl}#bootstrap'
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

  Verbatim configuration for individual components that have no specific option may be specified in the `component`
  module:

  ```nix
  {
    packages = {
      core = {
        library = {
          component = {
            other-modules = ["Prelude"];
          };
        };
      };
    };
  }
  ```

  '';

  cabalOptionsHeader = ''
  These are the Cabal options for packages and components that can be specified at any level.
  '';

  package = ''
  ### Package configuration {#package-conf}

  Packages may contain multiple components: an optional library and any number of sublibraries, executables, test suites
  or benchmarks.

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

  Multiple libraries are supported with the same syntax as other components.
  You can depend on them with the dependency string `<pkg>:<lib>`; when depending from another package, the library must
  have `public = true;` set.

  '';

  ifd = ''
  ## Multiple systems and IFD {#ifd}

  A set of flake outputs is defined for a specific architecture/system combination like `x86_64-linux`, which is the
  second level key in the `outputs` set.
  When a flake command like `nix build` is executed, Nix determines the system identifier matching the current machine
  and looks up the target in that set, like `packages.x86_64-linux.parser`.
  This means that you have to define a (not necessarily) identical output set for each architecture and system you would
  like to support for your project.

  While Hix takes care of structuring outputs for all supported [systems](#opt-general-systems) (via
  [flake-utils](https://github.com/numtide/flake-utils)), this feature comes with a significant caveat:
  When any part of an output set cannot be purely evaluated because it imports a file that was created by a derivation
  in the same evaluation (which is called Import From Derivation, IFD), evaluating this output will cause that
  derivation to be built.

  This becomes a critical problem when running the command `nix flake check`, which is a sort of test suite runner for
  flakes, because it evaluates *all* outputs before running the checks.
  Hix defines checks for all packages and GHC versions, so it is generally desirable to be able to run this command in
  CI.
  Unfortunately, `cabal2nix` (which generates all Haskell derivations) uses IFD.

  ### On-the-fly derivations {#no-ifd}

  To mitigate this problem, Hix can generate derivations in a similar manner to `cabal2nix`, controlled by the option
  [](#opt-general-ifd) (on by default).
  For local packages, this is rather straightforward – Hix can use the package config to determine the derivations
  dependencies and synthesize a simple derivation.

  It gets more difficult when [overrides](#ghc) are used, since these are usually generated from Hackage or flake
  inputs, where the only information available is a Cabal file.
  In order to extract the dependencies, we would either have to run a subprocess (via IFD) or implement a Cabal config
  parser in Nix (which has [also been done](https://github.com/cdepillabout/cabal2nixWithoutIFD)).

  As a pragmatic compromise, Hix instead splits the override derivation synthesis into two separate passes:

  - The flake app `gen-overrides` collects all `cabal2nix` overrides and stores their derivations in a file in the
    repository.
  - Whenever a build is performed afterwards, it loads the derivations from that file, avoiding the need for IFD!

  In order to use this feature, the option [](#opt-general-gen-overrides.enable) must be set to `true`.

  The derivations can be written to the file configured by [](#opt-general-gen-overrides.file) with:

  ```
  $ nix run .#gen-overrides
  ```

  Of course this means that `gen-overrides` will have to be executed every time an override is changed, but don't worry
  – Hix will complain when the metadata doesn't match!

  ::: {.note}
  This feature is experimental.
  :::

  ::: {.note}
  If your configuration evaluates overridden packages unconditionally, the command will not work on the first execution,
  since it will try to read the file and terminate with an error.
  In that (unlikely) case, you'll have to set `gen-overrides.enable = false` before running it for the first time.
  :::

  Another potential source for `nix flake check` to fail is when an environment with a virtual machine is exposed as a
  shell.
  For that purpose, the option [](#opt-env-systems) can be used to define for which systems the environment should be
  exposed.
  '';

  ui = ''
  ## User Interface {#ui}

  Options for UI behavior:
  '';

  environments = ''
  ## Environments {#envs}

  Actions like building a derivation and running HLS or GHCi are executed in an environment, which is a set of
  configuration options consisting of:

  - A GHC at a specific version
  - A set of packages that include project dependencies and tools like Cabal and HLS
  - An optional set of services that may be run in a virtual machine, like database servers
  - Environment variables

  This example configuration defines an environment that uses GHC 9.4, adds `socat` to the packages in `$PATH` and runs
  a PostgreSQL server:

  ```nix
  {
    outputs = {hix, ...}: hix ({config, ...}: {
      envs.example = {

        ghc.compiler = "ghc94";

        buildInputs = [config.pkgs.socat];

        services.postgres = {
          enable = true;
          config = { name = "test-db"; };
        };

        expose.shell = true;

      };
    });
  }
  ```

  ### Using environments {#envs-use}

  An environment can be used in different ways: By a derivation, a devshell, or a [command](#commands).
  A derivation only uses the GHC, Cabal and the project dependencies as inputs, while a command execution, like GHCid,
  is preceded by booting a VM if any services are configured.
  For the latter, the environment provides a script that wraps the command with the startup/shutdown code for the VM and
  adds all build inputs to the `$PATH`.

  The simplest way to use an environment is as a devshell:

  ```
  $ nix develop .#example
  >>> Starting VM with base port 20000
  >>> Waiting 30 seconds for VM to boot...
  $ ghc --version
  The Glorious Glasgow Haskell Compilation System, version 9.4.3
  $ psql "host=localhost port=25432 user=test-db password=test-db dbname=test-db" -c 'select 1'
  ?column?
  ----------
          1
  (1 row)
  $ exit
  >>> Killing VM
  ```

  ::: {.note}
  Using `nix develop -c some-command` to execute a shell command in an environment will fail to stop the VM afterwards.
  Use a [command](#commands) for one-shot executions.
  :::

  The default environment is called `dev` and is used for everything that doesn't have a custom environment.
  For instance, the default devshell uses this environment when entered with `nix develop` without an explicit argument.
  Running `cabal build` in that shell will use the configured GHC.

  All environments are exposed in the flake output namespace `devShells` (so they can be used with
  `nix develop .#<env-name>`), unless the option [](#opt-env-expose-shell) is set to `false`.

  ### Configuring GHC {#ghc}

  The most important part of an environment is the associated GHC.
  As demonstrated in the above example, the option `ghc.compiler` in an environment is used to select the package set
  corresponding to a version.
  These package sets are predefined by nixpkgs – each snapshot has a certain number of GHC versions with packages
  obtained from Stackage and Hackage.

  The GHC executable provided by an environment comes preloaded with all dependencies declared in the project's Cabal
  files (which amounts to those declared in `packages`).
  When running `ghci`, those dependencies are importable.

  The default package set doesn't always provide the version of a package that the project requires – for example,
  sometimes you want to use a newer version than the stable one in a Stackage snapshot.
  For this purpose, Hix offers a DSL for package overrides:

  ```nix
  {
    overrides = {hackage, ...}: {
      streamly = hackage "0.9.0" "1ab5n253krgccc66j7ch1lx328b1d8mhkfz4szl913chr9pmbv3q";
    };
  }
  ```

  Overrides are defined as a function that produces a set mapping package names to dependency specifications and takes
  as its argument a set of combinators and metadata for declaring those specifications, like the `hackage` combinator
  used above that takes a version and Nix store hash to fetch a package directly from Hackage.
  They can be specified at different levels, like dependencies: At the top level for all environments, in each
  individual environment, and on the `ghc` module in an environment (although the latter is populated by Hix with the
  merged global and local overrides).

  ::: {.note}
  nixpkgs' shipped GHC package sets come with a few special derivations whose attribute names are suffixed with a
  concrete version, like `Cabal_3_10_2_1`, to be used as overrides for packages with incompatible requirements.
  If you create a Hackage override for such a package, there's a chance that it will result in an `infinite recursion`
  error.
  The precise reason for this is not clear to me yet, but it can be avoided by using that special derivation, if it is
  compatible with your dependency set:
  ```
  ```
  {
    overrides = {super, ...}: {
      broken-dep = super.broken-dep_1_2_3;
    };
  }
  :::

  ### Override combinators {#overrides-combinators}

  There are different classes of combinators.
  The most fundamental ones are used to declare package sources:

  - `hackage` takes two arguments, version and hash, to pull a dependency directly from Hackage.

  - `source.root` takes a path to a source directory (like a flake input) and builds the dependency from its contents.

  - `source.sub` is the same as `source.root`, but takes a second argument describing a subdirectory of the path.

  - `source.package` is the same as `source.sub`, but it prepends `packages/` to the subdirectory.

  - `hackageAt` is like `hackage`, but takes an additional first argument specifying the address of the server.

  - `hackageConf` is like `hackageAt`, but instead of an address, it takes the name of an item in
    [](#opt-hackage-hackage.repos).
    If a server with the key `hix-override` is configured, the combinator `hackage` will automatically use it instead of
    nixpkgs' built-in logic.

  - `disable` or `null` (the literal) results in the final derivation being assigned `null`.
    This may be useful to force the dependency by that name to be provided by some other mechanism; for example, boot
    libraries are available in the compiler package and are set to `null` in nixpkgs.

  When an override is defined using `source` combinators, the derivation is generated by `cabal2nix`.
  Consider the following override for a package named `dep` that uses a flake input as the source path:

  ```
  {
    inputs = {
      hix.url = "...";
      dep.url = "...";
    };
    outputs = {hix, dep, ...}: hix {
      overrides = {source, ...}: {
        dep = source.root dep;
      };
    };
  }
  ```

  The implementation of `source.root` roughly consists of the function call
  `haskellPackages.callCabal2nix "dep" dep { <optional overrides>; }` which invokes `haskellPackages.haskellSrc2nix`,
  which executes the program `cabal2nix`, which in turn analyzes the `dep.cabal` in the source tree of the flake input
  and  generates a function that looks like this:

  ```
  {
    cabal2nix_dep = {mkDerivation, aeson, base, pcre}: mkDerivation {
      pname = "dep";
      src = <path passed to source.root>;
      libraryHaskellDepends = [aeson base];
      librarySystemDepends = [pcre];
      ...
    }
  }
  ```

  Its arguments consist of `dep`'s Haskell and system dependencies as well as the function `mkDerivation`.
  All of these are ultimately provided by the GHC package set, and `callCabal2nix` wraps this generated function with
  the common
  [`callPackage`](https://nixos.org/manual/nixpkgs/unstable#function-library-lib.customisation.callPackageWith)
  pattern, which allows individual arguments to be overridden.
  For example, to sketch a simplified representation of how our local package would be merged into the nixpkgs GHC
  package set, the above function would be called like this:

  ```
  let
    callPackage = lib.callPackageWith ghc;
    ghc = {
      # These two are defined in nixpkgs.

      # In `pkgs/development/haskell-modules/make-package-set.nix`
      mkDerivation = pkgs.callPackage ./generic-builder.nix { ... };

      # In the generated `pkgs/development/haskell-modules/hackage-packages.nix`
      aeson = callPackage aesonDrv {};

      ...

      api = callPackage cabal2nix_dep {
        aeson = someCustomAesonDerivation; # Haskell package override
        pcre = pkgs.pcre; # System package override
      };
    };
  in ghc
  ```

  Now, this is already very convoluted, and the real thing is even worse; but you can see:

  - The `callPackage` variant used for the Haskell derivations is created with `lib.callPackageWith` by recursively
    providing the set `ghc`, which consists of all the lazy attributes that are themselves constructed with that same
    function.
    Just like most of nixpkgs, the fixpoint of this computation is the final value of `ghc`, allowing each package to
    depend on others from the same set without having to provide the deps directly.
  - `mkDerivation` is _also_ created with a `callPackage` variant, but it's the top-level nixpkgs set that provides the
    values.
  - `mkDerivation` will be passed to the calls in `aeson` and `dep` automatically.
  - The call to our `cabal2nix` derivation created by `source` would get the sibling attribute `aeson`, but the second
    argument to `callPackage` here contains some custom override (as well as another for `pcre`), which takes precedence
    over the "default arguments" in the recursive set.
    Now, of course, these overrides need to be specified somehow, and the mechanism for that is the third argument to
    `callCabal2nix` that is labeled with `<overrides>` in the above example.
    However, this argument is not directly exposed in `source.root`.
    Instead, another type of override combinator provides that functionality, described below (`cabal2nixOverrides`).

  #### Other combinators {#overrides-combinators-other}

  The second class consists of "transformers", which perform some modification on a derivation.
  They can either be applied to a source combinator or used on their own, in which case they operate on whatever the
  previous definition of the overridden package is (for example, the default package from nixpkgs, or an override from
  another definition of `overrides`).

  Transformers also compose – when using multiple of them in succession, their effects accumulate:

  ```nix
  {
    overrides = {hackage, notest, nodoc, configure, jailbreak, nobench, ...}: {
      # Fetch streamly from Hackage and disable tests and haddock
      streamly = notest (nodoc (hackage "0.9.0" "1ab5n253krgccc66j7ch1lx328b1d8mhkfz4szl913chr9pmbv3q"));

      # Use the default aeson package and disable tests
      aeson = notest;

      # Disable version bounds, tests, benchmarks and Haddock, and add a configure flag
      parser = configure "-f debug" (jailbreak (notest (nobench nodoc)));
    };
  }
  ```

  The available transformers are:

  - `unbreak` – Override nixpkgs' "broken" flag
  - `jailbreak` – Disable Cabal version bounds for the package's dependencies
  - `notest` – Disable tests
  - `nodoc` – Disable Haddock
  - `bench` – Enable benchmarks
  - `nobench` – Disable benchmarks
  - `minimal` – Unbreak and disable profiling libraries, Haddock, benchmarks and tests
  - `fast` – Disable profiling and Haddock
  - `force` – Unbreak, jailbreak and disable Haddock, benchmarks and tests
  - `noprofiling` – Disable profiling libraries for the package
  - `profiling` – Enable executable profiling for the package
  - `configure` – Add a Cabal configure CLI option
  - `configures` – Add multiple Cabal configure CLI options (in a list)
  - `enable` – Enable a Cabal flag (`-f<flag>`)
  - `disable` – Disable a Cabal flag (`-f-<flag>`)
  - `ghcOptions` – Pass GHC options to Cabal (`--ghc-options`)
  - `override` – Use `overrideCabal` to modify the arguments passed to the Haskell-specific derivation builder (the
    function `mkDerivation` explained below).
    Similar to the more general `stdenv.mkDerivation` used by most nixpkgs packages (and wrapped by the Haskell
    builder).
  - `overrideAttrs` – Call the function `overrideAttrs` that is added by `stdenv.mkDerivation`.
    This allows overriding the arguments that the Haskell builder passes to the `stdenv` builder.
  - `buildInputs` – Add generic build inputs, like C libraries.
    Can be a function, in which case it is applied to `pkgs`.

  The third class consists of "options", which modify the behavior of other override combinators:

  - `cabal2nixOverrides` – An attrset used as the third argument of `callCabal2nix` when called by `source` combinators,
    as described in the example in the previous section, used as follows:

    ```
    {
      overrides = {cabal2nixOverrides, source, pkgs}: {
        dep = cabal2nixOverrides { aeson = ...; pcre = pkgs.pcre.override {...}; } (source.root dep);
      }
    }
    ```
    This would result in both `aeson` and `pcre` being overridden, for the derivation of `dep`, by the packages supplied
    explicitly.
  - `cabal2nixArgs` – CLI options for `cabal2nix`.

    `cabal2nix` doesn't have many options that would make a lot of sense in this context, but one is particularly
    useful:

    Like `cabal-install`, `cabal2nix` recognizes the option `-f` to enable or disable Cabal flags, which are often used
    to configure optional dependencies.
    The problem with passing these only to Cabal in the final derivation is that the function generated by `cabal2nix`
    will require exactly those dependencies that were enabled when it was invoked.

    Imagine that our example package has a Cabal flag `large-records` that controls two effects: an additional
    dependency on the package `large-records` and some CPP-guarded code that uses this library.
    Disabling a flag with `disable "large-records" (source.root dep)` will ultimately cause Cabal to build `dep` without
    `large-records` visible in the GHC package DB, as well as ignore the additional code, but the derivation will still
    have the dependency on `large-records`, since that is not interpreted by Cabal.
    However, `cabal2nixArgs "-f-large-records" (source.root dep)` will avoid the dependency entirely.

  Finally, there are some special values that are injected into the override function:

  - `self` and `super` – The final and previous state of the package set, as is common for nixpkgs extension functions
  - `pkgs` – The nixpkgs set
  - `keep` – Reset the previous combinators and use the default package
  - `hsLib` – The entire nixpkgs tool set `haskell.lib`
  - `hsLibC` – The entire nixpkgs tool set `haskell.lib.compose`.
    This is preferable to `hsLib`, but for historic reasons that one ended up in here first, and it's gonna stay for
    compatibility.
  - `compilerName` – The `name` attribute of the GHC package
  - `compilerVersion` – The `version` attribute of the GHC package

  #### Transitive overrides {#overrides-transitive}

  Overrides can be exported from the flake in order to reuse them in other projects.
  When a downstream flake has project `foo` as an input, setting `deps = [foo]` will cause `foo`'s overrides to be
  incorporated into the local ones.
  Furthermore, the option `depsFull` will additionally include `foo`'s local packages in the overrides:

  ```nix
  {
    inputs.dep1.url = github:me/dep1;
    inputs.dep2.url = github:me/dep2;

    outputs = { hix, dep1, dep2, ... }: hix {
      deps = [dep1];
      depsFull = [dep2];
    };
  }
  ```

  ### Configuring nixpkgs {#ghc-nixpkgs}

  The GHC package set uses the same nixpkgs snapshot that is also used for Hix internals, which is configured as a flake
  input of the Hix repository.
  You can override this globally:

  ```nix
  {
    inputs.hix.inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
  }
  ```

  In order to avoid incompatiblities with the Hix internals, it might be advisable to only override the nixpkgs used by
  GHC:

  ```nix
  {
    inputs.hix.url = "github:tek/hix";
    inputs.ghc_nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";

    outputs = { hix, ghc_nixpkgs, ... }: hix {
      envs.dev.ghc.nixpkgs = ghc_nixpkgs;
    };
  }
  ```

  Since the usage of nixpkgs within the library is tightly interwoven with the GHC package set, this might have a slight
  potential for breakage, but (like the global variant) it should be minimal.

  ## Commands {#commands}

  Services in an environment are relevant when executing a command, which consists of an arbitrary shell script
  associated with an environment.
  When the command is executed, the environment's `code` option will be run beforehand, which boots the VM with the
  services and sets up an exit hook for shutting it down.
  `code` is a script assembled from other options, notably `setup-pre`, `setup`, `exit-pre` and `exit`.

  A command can be defined as follows:

  ```nix
  {
    commands.integration-test = {
      env = "example";
      command = "cabal test api-integration";
    };
  }
  ```

  This assumes that your project contains some Cabal component named `api-integration` that needs a PostgreSQL server
  running.
  When executing this command, the setup code from the previously defined environment `example` will be executed,
  starting the virtual machine, before running the specified Cabal command line:

  ```
  nix run .#cmd.integration-test
  ```

  ### Component-dependent environments {#commands-component-env}

  When the command option `component` is set to `true`, the command will take two argument lists, separated by a `--`.
  The first batch of arguments is passed to the Hix CLI to select the environment, the second one is assigned to the
  environment variable `$cmd_args` to be used by the command script.

  This allows the user to select an environment dynamically from the command line, without having to statically
  associate it in the Nix config or use complex expressions with nested invocations of Nix, realized by encoding all
  components and environments as JSON and extracting the environment runner based on the CLI arguments.
  The primary use case for this is to use a different environment when running a GHCid test, like running a database
  server for integration tests.

  There are three alternative selection methods, illustrated by this example:

  ```
  ${exampleFile "doc-env-selection/flake.nix"}
  ```

  The first method is to use the flake app path to select an environment by name, then append the command name:

  ```
  $ nix run .#env.three.number
  3
  ```

  The second method is to select a component by its package name or directory and component name or source directory:

  ```
  $ nix run .#cmd.number -- -p api -c server
  $ nix run .#cmd.number -- -p packages/api -c app
  2
  ```

  The third method is to specify a Haskell source file and let Hix figure out which component it belongs to:

  ```
  $ nix run .#cmd.number -- -f /path/to/packages/api/test/NumberTest.hs
  2
  ```

  This method needs to know the root directory of the project, which is determined by searching for `flake.nix` with a
  fallback to the current working directory.
  The root directory may also be specified explicitly using the CLI option `--root`.

  If no selection arguments are given, the command's default environment is used:

  ```
  $ nix run .#cmd.number
  1
  ```

  ### Built-in commands {#commands-builtin}

  Hix provides two special commands for executing a function in GHCi or GHCid.

  The `ghcid` command in particular is highly useful for repeatedly executing a test whenever a source file is written.
  Compilation is very fast since GHCi doesn't need to link the executable and doesn't perform optimization in the
  default mode, leading to sub-second feedback loops on small projects (or dependency closures).

  ```
  nix run .#ghci -- -p root -r server
  nix run .#ghcid -- -p root -t test_server -r hedgehog-unit
  ```

  Their interface is mostly identical to the generic commands described above, while taking three additional, optional
  command line options:

  - The name of a module (`-m`), defaulting to `Main`.
    This module is loaded in GHCi and hence only its dependencies are (re)compiled.

  - The name of a Haskell test function (`-t`) that will be called after the module was loaded successfully.
    See next bullet for defaults.

  - The name of an attribute in the Hix option `ghci.run` (`-r`).
    This option contains a Haskell expression for each key which will be executed after the module was loaded
    successfully.
    If a test function was specified, this expression will be applied to it.
    If the runner wasn't specified, the test function will be run directly.
    If neither this nor the test function was specified, the `ghci` command will drop to interactive mode directly while
    the `ghcid` command defaults to `main`.

  For example, the built-in `hedgehog-unit` entry in `ghci.run` has the value `check . withTests 1 . property . test`.
  When specifying the name of a Hedgehog test like in the example above, the evaluated epression will be:

  ```
  ghci> (check . withTests 1 . property . test) test_server
  ```

  You can specify arbitrary additional command line arguments for GHCi and GHCid with `--ghci-options` and
  `--ghcid-options`.

  Another built-in command is `run`, which executes an arbitrary shell command in an environment:

  ```
  $ nix run .#env.ghc92.run -- "ghc --version"
  The Glorious Glasgow Haskell Compilation System, version 9.2.4
  ```

  ::: {.note}
  If you depend on the GHC library package `ghc`, you'll have to set `ghci.args = ["-package ghc"];`.
  Otherwise it won't be visible, due to a bug.
  :::

  ## Services {#services}

  Services used in environments and commands can be user-defined in a flake:

  ```
  {
    services.http = {
      nixos.services.nginx = {
        enable = true;
        virtualHosts.localhost.locations."/test".return = "200 Yay!";
      };
      ports.nginx = { host = 2000; guest = 80; };
    };

    envs.test = {
      basePort = 10000;
      services = { http = { enable = true; }; };
    };
  }
  ```

  This defines a service named `http` that runs an nginx with a single endpoint at `/test` and exposes it in the host
  system at port offset 2000 (relative to an environment's `basePort`).
  The environment `test` references the service, so when a command uses this environment, a VM will be started:

  ```
  nix run .#env.test.ghcid -- -p root
  ```

  Since the environment uses `basePort = 10000`, the nginx server will listen on port 12000.
  You can refer to the effective port from other options with `config.envs.hostPorts.nginx` (the attribute name in
  `ports`).

  Hix provides built-in services, like the previously mentioned PostgreSQL server, that have specialized configuration
  options.
  They can be configured in the same option as the definition of a new service, still allowing the specification of
  additional NixOS config as described before, in `services.<name>.nixos`.

  Furthermore, an environment may provide Hix config overrides in `envs.<name>.services.<name>.config` that is combined
  with the config in `services.<name>`.

  ```nix
  {
    outputs = {hix, ...}: hix ({config, ...}: {

      services.postgres = {
        # Add NixOS config to the default config computed by Hix
        nixos.users.users.postgres.extraGroups = ["docker"];

        # Configure PostgreSQL specifically, used by `services.postgres.nixos-base` internally
        creds.user = "root";
      };

      envs.example = {

        services.postgres = {
          # Declare that this environment uses `config.services.postgres` above
          enable = true;

          # Add overrides for the configuration in `config.services.postgres`
          config = { name = "test-db"; };
        };

      };

    });
  }
  ```

  In order to define a service with specialized config, an entry in the option `internal.services.myservice` must be
  provided that contains a module with option declarations.
  This option has type `deferredModule`, which means that it's not evaluated at the definition site, but used in a magic
  way somewhere else to create a new combined module set consisting of all the configs described before.

  You can log into a service VM via ssh.
  The default configuration sets the root password to the empty string and exposes the ssh port in the host system at
  `basePort + 22`:

  ```
  ssh -p 10022 root@localhost
  ```

  ### Defining modular services {#services-define}

  A service can be defined in [](#opt-general-services) with plain NixOS config, but it is useful to allow the service
  to be specially configurable.
  For that purpose, the value assigned to the entry in  [](#opt-general-services) should be a full module that defines
  options as well as their translation to service config:

  ```
  {
    services.greet = ({config, lib, ...}: {

      options.response = lib.mkOption {
        type = lib.types.str;
        default = "Hello";
      };

      config = {
        ports.greet = { guest = 80; host = 10; };
        nixos-base.services.nginx = {
          enable = true;
          virtualHosts.localhost.locations."/greet".return = "200 ''${config.response}";
        };
      };

    });

    envs.bye = {
      services.greet = {
        enable = true;
        config.response = "Goodbye";
      };
    };
  }
  ```

  This defines a service running nginx that has a single endpoint at `/greet`, which responds with whatever is
  configured in the service option `response`.
  The environment `bye` uses that service and overrides the response string.

  Any option defined within `services.greet.options` is configurable from within the environment, and the values in
  `services.greet.config` correspond to the regular service options.

  '';

  hls = ''
  ## Haskell Language Server {#hls}

  Hix provides a flake app for running HLS with the proper GHC (for the `dev` env) and the project dependencies:

  ```
  nix run .#hls
  ```

  In order to use it with your IDE, you need to specify the command in the editor configuration as: `nix run .#hls --`,
  since `nix` consumes all options until a `--` is encountered, and only passes what comes afterwards to the program
  (which in this case would be `--lsp`).

  This app corresponds to the [command](#commands) named `hls`, which uses the `dev` environment but gets the HLS
  executable from a special environment named `hls`.
  This allows the HLS package and its dependencies to be configured separately from the project dependencies.
  For example, to use the package exposed from the HLS flake:

  ```
  {
    inputs.hls.url = "github:haskell/haskell-language-server?ref=1.9.0.0";

    outputs = {hix, hls, ...}: ({config, ...}: {
      envs.hls.hls.package = hls.packages.''${config.system}.haskell-language-server-925;
    });
  }
  ```

  Additionally, all other environments can expose HLS as well:

  ```
  {
    envs.ghc94.hls.enable = true;
  }
  ```

  ```
  nix run .#env.ghc94.hls
  ```

  This is disabled by default to avoid building HLS for environments whose GHCs don't have its derivation in the Nix
  cache.

  Since the dev environment exposes HLS by default, the executable (`haskell-language-server`) is in `$PATH` in the
  default devshell, so it can also be run with `nix develop -c haskell-language-server`.
  '';

  compat = ''
  ## GHC version checks {#checks}

  The environments created for each entry in [](#opt-general-ghcVersions) are intended primarily as a CI tool to ensure
  that the project builds with versions other than the main development GHC.
  For that purpose, the `checks` output contains all packages across those environments, which can be built with:

  ```
  nix flake check
  ```
  '';

  upload = ''
  ## Hackage upload {#upload}

  Hix provides flake apps that run the flake checks and upload package candidates, releases or docs to Hackage:

  ```
  nix run .#candidates
  nix run .#release
  nix run .#docs
  ```

  If `versionFile` is set, the script will substitute the `version:` line in that Cabal file after asking for
  the next version.
  If you use `nix run .#gen-cabal` to maintain the Cabal files, this should be a `.nix` file containing a string that's
  also used for [](#opt-cabal-version):

  ```
  {
    packages.parser = {
      cabal.version = import ./parser-version.nix;
      versionFile = ./parser-version.nix;
    };
  }
  ```

  The options [](#opt-hackage-hackage.versionFileExtract) and [](#opt-hackage-hackage.versionFileUpdate) can be
  customized to allow for arbitrary other formats.

  The command line option `--version`/`-v` may be used to specify the version noninteractively.
  Furthermore, if a package name is specified as a positional argument, only that package will be uploaded.

  For example, to publish `parser` at version `2.5.0.1`:

  ```
  nix run .#release -- parser -v 2.5.0.1
  ```

  The upload command will use your global Cabal config to obtain credentials, please consult the Cabal docs for more.

  The options module `hackage.hooks` provides a way to execute scripts at certain points in the release process.
  '';

  tags = ''
  ## CTags {#tags}

  Hix exposes an app that runs [thax](https://github.com/tek/thax) to generate a CTags file covering all dependencies
  and the local packages:

  ```
  nix run .#tags
  ```

  This will result in the creation of the file `.tags`.
  '';

  cross = ''
  ## Cross-compilation and static linking {#cross}

  All package outputs can be cross-compiled with the following syntax:

  ```
  nix build .#parser.cross.musl64
  ```

  In addition, the package may also be linked statically against its Haskell dependencies:

  ```
  nix build .#parser.cross.musl64.static
  ```

  Note that this does not result in a static binary – it will still be linked dynamically against libc.

  For more elaborate cross-compilation setups, each GHC can be configured to use a [cross pkgs set](#opt-ghc-crossPkgs):

  ```
  {
    envs.dev.ghc.crossPkgs = config.envs.dev.ghc.pkgs.pkgsCross.musl64;
  }
  ```

  For `musl`, there are two native package sets in nixpkgs that are supported by Hix:

  ```
  nix build .#parser.musl
  ```

  This will result in a binary that's similar to `.#parser.cross.musl64.static`.

  For a fully static build, you can use the `static` attribute:

  ```
  $ nix build .#parser.static
  $ file result/bin/parser
  result/bin/parser: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), statically linked, stripped
  ```

  ### AppImage bundles {#appimage}

  Hix can create more portable distributable bundles by using [nix-appimage](https://github.com/ralismark/nix-appimage)
  to generate AppImage executables, exposed in the following flake apps:

  ```
  nix run '.#appimage' # The default executable from the default package
  nix run '.#<pkgname>.appimage' # The default executable from the specified package
  nix run '.#<exename>.appimage' # The specified executable from whatever package defines it
  nix run '.#env.<envname>.[<pkg/exe>.]appimage' # The same as above, but from the specified env
  ```

  This will print a store path:

  ```
  >>> AppImage bundle for <exename> created at:
  /nix/store/bxbcp9mk9rf0sjg88hxsjqzpql5is280-<exename>-0.1.0.0-x86_64.AppImage
  ```

  '';

  managed = ''
  ## Automatic dependency management {#managed}

  Hix provides some functionality for adapting and verifying dependency bounds.

  ::: {.note}
  This feature is new, so there are likely many edge cases that have not been tested.
  :::

  ### Upper bounds and latest versions {#latest}

  If the option [](#opt-managed-managed.enable) is enabled, the flake will expose an environment named `latest`
  and an app named `bump`.

  Running this app with `nix run .#bump` will fetch the newest versions of all dependencies and create a file in the
  project at the path configured by [](#opt-managed-managed.file), containing updated dependency version ranges
  that include the new version.

  For each dependency, this app will build all of the project's packages and omit the version update if the build fails.
  When generating cabal files or derivations, the version ranges from this file will override those from the flake.

  Additionally, the app will add overrides to the same file that select the newest version for the environment `latest`,
  so that the `dev` environment (and all others) will still use the default versions from nixpkgs (plus regular
  overrides), making the `latest` environment the testing ground for bleeding-edge dependency versions.

  You can change this behavior to apply to other environments by setting [](#opt-env-managed) to `true` and running `nix
  run .#env.<name>.bump` instead.

  If [](#opt-managed-managed.check) is `true` (the default), the derivations with latest versions will be added
  to the `checks` output, so your CI may depend on them.

  ### Lower bounds {#lower}

  If the option [](#opt-managed-managed.lower.enable) is enabled, the flake will expose an environment named `lower` and
  an app named `lower`.
  The app executes a subset of three stages based on the current state, `init`, `optimize` and `stabilize`, that are
  also available as subcommands of this app (by running e.g. `nix run .#lower optimize`).
  The mode of operation is similar to `bump`, with the most crucial difference being that it manipulates the lower
  bounds of dependencies.

  The operational details of this app that are most important for a user are that `optimize` determines the lowest
  possible bounds with which the project builds, and that `stabilize` attempts to repair them after the code was changed
  in a way that invalidates them, e.g. by importing a name that wasn't available in a dependency at the version in the
  lower bound.
  This allows the bounds validation to be integrated into the development process by running
  `nix run .#lower -- --stabilize` as a pre-commit hook or a pre-release CI job.

  Since older versions require boot packages from older GHCs, it is advisable to use the oldest GHC available.
  See the option [](#opt-managed-managed.lower.compiler) for more information.
  The default is to use the first version in [](#opt-general-ghcVersions).

  #### Details {#lower-details}

  The first stage of `lower` is called `init`, and it attempts to determine _initial_ lower bounds, which consist of the
  lowest version in the highest major that builds successfully with the environment's GHC.
  For example, if a dependency has the majors 1.4, 1.5 and 1.6, and all versions greater than 1.5.4 fail to build, the
  initial lower bound will be 1.5.0.
  Repeatedly executing this stage will only compute initial bounds for dependencies that don't have any yet (unless
  `--reset` is specified).

  The purpose of this step is preparation for the other two apps: The initial bounds will be sufficiently recent that
  the project won't break with these versions after changes with a high probability.
  The initial versions are stored in the managed state file along with the bounds.

  Executing the app with a subcommand, `nix run .#lower init`, will only run this stage; otherwise, the next stage will
  depend on the final state and the outcome of `init`.

  If the specified dependencies have lower bounds configured in the flake, they will be ignored.

  After the lower bounds have been initialized, or if the first stage is skipped due to all dependencies having bounds,
  the app will build the current state to decide what to do next.
  If the build succeeds, it will continue with the `optimize` stage, otherwise it will run `stabilize`, although this
  requires the user to pass `--stabilize` in the command line, since it is an expensive operation with a high failure
  rate.

  The `optimize` stage will iterate over the majors of all dependencies, building all versions in each of them until it
  finds a working one, and terminating when an entire majors fails after at least one working version had been found.
  E.g. if a package has the majors 1.1, 1.2, 1.3, 1.4 and 1.5, and some versions in 1.3 and 1.4 build successfully, but
  none in 1.3 do, then the lowest version in 1.4 will be the optimized lower bound.

  Before the `stabilize` stage is executed, the app will first build the project with the versions that form the initial
  lower bounds.
  If that fails, it will refuse to continue, and require the user to fix the situation manually (the easiest first step
  would be to run `nix run .#lower -- --reset`).
  Otherwise, it will build the project with the initial lower versions and the optimized lower bound of the first
  dependency, continuing upwards in the version sequence until a working version is found.
  Then the same process is performed with the next dependency, keeping the stabilized bounds of all previously processed
  dependencies.

  ### Target sets {#managed-targets}

  In the default mode, the managed-deps apps operate on the entirety of local packages, finding new bounds that work for
  all packages.
  This might be undesirable – some of the packages might have stricter version requirements than others, or they might
  be completely independent from each other.

  For that purpose, the option [](#opt-managed-managed.sets) may be used to specify multiple sets of packages that are
  processed independently.

  The simplest variant, with the value `managed.sets = "each";`, is to create one app and one env for each package, so
  that you would run `nix run .#bump.api` to bump only the package `api`, with a flake check created as `bump-api`.

  More granular sets can be specified like this:

  ```
  {
    main = ["core" "api" "app"];
    other = ["docs" "compat"];
  }
  ```

  Now the apps have paths like `nix run .#lower.init.main`, while the checks use the schema `lower-main-api`.

  The default value for `sets` is `"all"`, which processes all packages at once as described above.
  If your packages are dependent on each other, this might be more desirable, since it reduces the build time.

  ### Fine tuning {#managed-tuning}

  This feature is incredibly complicated and suffers from vulnerabilities to myriad failure scenarios, some of which
  might be mitigated by configuration.

  The app uses the Cabal solver to optimize the selection of overrides, which requires two package indexes:

  - A source package database of available packages with their versions and dependencies, which is read from Hackage.
  - An installed package index of concrete versions of packages that are considered to be bundled with the compiler,
    which is provided as the executable of a nixpkgs [GHC](#ghc) with a selection of packages.

  The latter of those is a very difficult mechanism that serves multiple independent purposes.

  In order to make installed packages visible to Cabal, the Nix GHC has to be outfitted with a package DB, which is the
  same structure used for development shells with GHCi, obtained by calling `ghcWithPackages` and specifying the
  dependencies from the flake config as the working set.
  This results in a GHC executable that has those dependencies "built in", meaning that you can just import them without
  any additional efforts, and the solver can query them by executing `ghc-pkg list`.

  Since we're required to ultimately build the project with Nix, a major concern is to avoid rebuilding dependencies
  that are already available in the central cache.
  If a version selected by the solver matches the one present in the GHC set, we therefore don't want to specify an
  override (to pull the version from Hackage and build it), since that version is very likely cached.

  However, the GHC package set from nixpkgs always has the potential of containing broken packages, most often due to
  incompatible version bounds, or simply because a package fails to build with a bleeding-edge GHC (not to mention any
  requirements for custom build flags that your project might have).
  Even though this is a problem that the Cabal solver is intended to, uh, solve, the GHC set must be fully working
  before starting the app, since that is how Nix works – we pass a store path to the GHC with packages to the app as a
  CLI option (roughly).

  Now, the set of installed packages should resemble the "vanilla" Nixpkgs as closely as possible because of the
  aforementioned benefit of avoiding builds of cached versions, but if a broken package requires on override, parts of
  the package set will differ from the vanilla state!

  To complicate matters even more, the same issue arises twice when building the project with the computed overrides –
  once when the app tests the build, and again when the final state has been written and the environment is built by a
  flake check or manual invocation.

  At least for test builds and bound mismatches there's a partial mitigation in place, since the app forces a jailbreak
  (removal of most bounds) of all overridden dependencies, but this only covers a tiny part of the problem space.

  For more direct control, you can specify overrides in the flake config.
  However, since the solver is supposed to work on vanilla package sets, most of the usual override sources are ignored,
  so there is a special option for this purpose.
  In addition, a project might contain several managed environments with different requirements, so each must be
  configurable individually, but we also don't want to be forced to specify a common override multiple times.

  To achieve this, the modules [](#opt-managed-managed.envs), [](#opt-managed-managed.latest.envs) and
  [](#opt-managed-managed.lower.envs) allow you to specify configuration for all managed envs and all latest and lower
  envs, respectively.
  The option [](#opt-managed-managed.envs.solverOverrides) is used only for the solver package set (with the usual
  [](#overrides-combinators) protocol).
  The actual build overrides can be defined at [](#opt-managed-managed.envs.verbatim), which is equivalent to
  specifying regular env-specific overrides for all managed (or latest/lower) environments individually.
  Note that at the moment, the config in `latest`/`lower` completely overrides the more general one; they are not
  combined.

  For example, if your project depends on `type-errors`, which has an insufficient upper bound on a dependency in the
  current Nipxkgs set for the latest GHC that prevents the set from building, you might want to specify:

  ```
  {
    managed.latest.envs = {
      solverOverrides = {jailbreak, ...}: { type-errors = jailbreak; };
      verbatim.overrides = {jailbreak, ...}: { type-errors = jailbreak; };
    };
  };
  ```

  This will only use those overrides for `latest` envs used by `.#bump` – if some overrides should be used for lower
  bounds envs as well, you'd set this on `managed.envs` instead of `managed.latest.envs`.
  The "verbatim" config is copied to all envs that are generated for [](#managed-targets), so with the setup from that
  section, this config would be equivalent to:

  ```
  {
    envs = {
      latest-main.overrides = {jailbreak, ...}: { type-errors = jailbreak; };
      latest-other.overrides = {jailbreak, ...}: { type-errors = jailbreak; };
    };
  };
  ```

  ## Release maintenance {#managed-maint}

  ::: {.note}
  This feature is highly experimental and needs to be unlocked in the config:
  ```nix
  {
    ui.experimental.managed-maint = true;
  }
  ```
  :::

  Upper bounds updates executed on the main branch only affect future releases.
  Especially for people registered as package maintainers for Stackage, this does not solve the recurring chore of
  building previous releases with new bounds and creating Hackage revisions for them.

  Hix provides additional tools to automate this procedure:

  ```
  nix run .#maint -- [args]
  ```

  This app will run `.#bump` on special branches based on each package's latest tag, and upload Hackage revisions when
  bounds change.
  For better observability, this process can be split in two and supports Github Actions and PRs – you can run the
  following commands to generate workflows:

  ```
  step_run managed.gen.ga.maint
  step_run managed.gen.ga.revision
  ```

  The first workflow will run `.#maint` without publishing a revision, and create pull requests against release
  branches.
  Once one of these PRs has been merged, the second workflow will be triggered to publish the revision.

  Release branches use the naming schema `release/<package>/<version>` and will be created per-package, choosing base
  tags that are named either `<package>-<version>` or `<version>`.

  ### Hackage credentials {#managed-hackage}

  Revision uploads naturally require authentication, so you need to specify your hackage password somehow.
  The simplest way is to use [](#opt-hackage-hackage.repos._name_.password), but it can also be passed on the command
  line:

  ```
  nix run .#revision -- --branch 'release/api/0.5' --hackage="hackage.haskell.org:password:$hackage_password"
  ```

  The app will upload to all Hackage repos for which [](#opt-hackage-hackage.repos._name_.publish) is `true`, so you'll
  need to provide credentials for each of them.

  The user name can be specified in the same manner.

  Consider this more elaborate example:

  ```nix
  {
    hackage.repos = {

      # Default for `publish` is `true` for central Hackage
      "hackage.haskell.org" = {
        user = "deepspace-mining-corp";
        password = {
          type = "exec";
          value = "/path/to/password/script";
        };
      };

      prod = {
        location = "https://hackage.deepspace-mining.com:8080";
        publish = true;
        user = "ci";
        password = {
          type = "env-var";
          value = "hackage_password_prod";
        };
      };

      staging = {
        location = "http://hackage.staging:80";
        publish = true;
      };

    };
  }
  ```

  With this config, the app will execute the configured script to obtain the password for central Hackage, and fetch
  that for `prod` from the given environment variable.
  For `staging`, the credentials must be specified as CLI args:

  ```
  nix run .#revision -- --branch 'release/api/0.5' \
    --hackage="staging:user:ci-staging" \
    --hackage="staging:password:''${{ secrets.hackage_password }}"
  ```

  This example assumes a Github Actions environment and resembles the method used by the generated workflow described in
  the previous section.

  When this command line is executed, the app will publish revisions to all three configured Hackage repos.
  '';

  misc = ''
  ## Miscellaneous tools {#misc}

  ### Show overrides {#show-overrides}

  ```
  nix run .#show-overrides
  ```

  Prints all environments' overrides.

  ### Show config {#show-config}

  ```
  nix run .#show-config
  ```

  Prints the project's entire configuration.

  ### Show dependency versions {#dep-versions}

  ```
  nix run .#dep-versions
  nix run .#ghc96.dep-versions
  nix run .#env.ghc96.dep-versions
  ```

  Prints all components' dependencies and their actual versions in the dev environment, or the named environment in the
  second variant.
  The second variant is only available for envs with `expose.scoped = true;`, while the third variant is available for
  any env.

  ### Access to intermediate outputs {#intermediate-outputs}

  Packages and environments are subjected to several stages of transformation in order to arrive at the final flake
  outputs from the initial configuration obtained from module options.
  In order to make this process more transparent and flexible, in particular for overriding outputs without having to
  reimplement substantial parts of Hix's internals, intermediate data is exposed in the module arguments `project`,
  `build`, and `outputs`.
  This means that the module that constitutes the Hix flake config can access these arguments by listing them in its
  parameter set:

  ```
  {
    outputs = {hix, ...}: hix ({config, project, build, outputs, ...}: {
      packages.core.src = ./core;
      outputs.packages.custom-build = build.packages.dev.core.static.overrideAttrs (...);
    });
  }
  ```

  For now, these sets don't have a stable API, but here is a brief overview:

  - `project` contains values that are closely related to the config options they are computed from, to be used by a
    wide spectrum of consumers:
    - `project.base` contains the project's base directory in the nix store, which is either [](#opt-general-base) if
      that's non-null, or the directory inferred from [](#opt-package-src) if only the package config contains paths.
    - `project.packages.*.path` contains the relative path to a package, either given by [](#opt-package-relativePath)
      if that's non-null, or the directory inferred from `project.base`.

  - `build` contains the full set of derivations for each package in each env.
    Its API looks roughly like this:
    ```
    {
      packages = {
        dev = {
          core = {
            package = <derivation>;
            static = <derivation>;
            musl = <derivation>;
            cross = {
              aarch64-android = <derivation>;
              ...
            };
            release = <derivation>;
            ghc = { <package set used for this package> };
            cabal = { <cabal config for this package> };
            expose = true;
            executables = {
              <name> = {
                package = <derivation>;
                static = <derivation>;
                musl = <derivation>;
                app = <flake app>;
                appimage = <derivation>;
              };
              ...
            };
          };
          api = {
            ...
          };
          ...
        };
        ghc910 = {
          ...
        };
        ...
      };
      envs = {
        dev = {
          # Like above, but only for the main package of the set
          static = <derivation>;
          musl = <derivation>;
          cross = <attrs>;
          release = <derivation>;
          # Main executable of the main package
          api = <derivation>;
          # All executables of all packages, flattened
          executables = <attrs>;
        };
        ...
      };
      commands = {
        default = {
          # All built-in and custom commands using their default env env
          ghci = <derivation>;
          hls = <derivation>;
          ...
        };
        envs = {
          dev = {
            # All built-in and custom commands using this env
            ghci = <derivation>;
            hls = <derivation>;
            ...
          };
        };
      };
    }
    ```

  - `outputs` contains most of what will eventually end up in the flake outputs, keyed by output type.

  All of this data is also exposed in the flake outputs, and can therefore be inspected from `nix repl`:
  ```
  Welcome to Nix 2.18.5. Type :? for help.

  nix-repl> :load-flake .
  Added 19 variables.

  nix-repl> legacyPackages.x86_64-linux.project
  {
    # Values from `project`
    base = /nix/store/ga9ifpvqzqgi6sqcfqhdvhj0qmfms8hk-source;
    packages = { ... };
    # The sets `build` and `outputs`, as attributes of `project` for simpler scoping
    build = { ... };
    outputs = { ... };
    # Other internal data
    config = <full config>;
    ghc = <dev ghc set>;
    ghc0 = <dev ghc set without overrides>;
    pkgs = <dev nixpkgs>;
    show-config = <see above>;
  }

  nix-repl> legacyPackages.x86_64-linux.project.build.packages.dev.hix.package
  «derivation /nix/store/qys3qc4kyvfx6wlsqkvjxk40dyq28gl3-hix-0.7.2.drv»

  nix-repl> packages.x86_64-linux.hix
  «derivation /nix/store/qys3qc4kyvfx6wlsqkvjxk40dyq28gl3-hix-0.7.2.drv»
  ```
  '';

}
