{

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
  |`envs.dev.ghc.compiler`|`"ghc925"`|The attribute name of the GHC package set to use for development.|
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
        envs.dev.ghc.compiler = if config.system == "aarch64-linux" then "ghc902" else "ghc925";
      };
    in hix.lib.flake module;
  }
  ```

  For even more elaborate setups, `hix.lib.flake` also accepts a *list* of modules.
  '';

}
