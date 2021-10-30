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

For the explanation of other features, consider this example flake:

```nix
{
  description = "Spaceship";

  inputs.hix.url = github:tek/hix;
  inputs.polysemy-log.url = github:tek/polysemy-log;

  outputs = { hix, polysemy-log, ... }:
  let
    common = { hackage, jailbreak, ... }: {
      co-log-polysemy = jailbreak;
      polysemy = hackage "1.5.0.0" "1xl472xqdxnp4ysyqnackpfn6wbx03rlgwmy9907bklrh557il6d";
    };

    main = { hackage, source, ... }: {
      path = hackage "0.8.0" "0isldidz2gypw2pz399g6rn77x9mppd1mvj5h6ify4pj4mpla0pb";
      polysemy-log = source.sub polysemy-log "packages/polysemy-log";
    };

    compat = { hackage, only, ... }: {
      polysemy = hackage "1.6.0.0" "04bl0w7z35jh63jpy87sa1rrbgqhwn7c0pxsm5l3ww0pjnswkhjj";
    };

  in hix.flake {
    base = ./.;
    compiler = "ghc901";
    packages = {
      spaceship-core = ./packages/core;
      spaceship-api = ./packages/api;
    };
    main = "spaceship-api";
    overrides = [common main];
    compatOverrides = { all = [common]; ghc8107 = [compat]; };
    ghci.extraArgs = ["-fplugin=Polysemy.Plugin"];
    versionFile = "ops/hpack/shared/meta.yaml";
    ghcid.commands = pkgs: {
      dev-api = {
        script = ''
          :load Spaceship.Api.Dev
          :import Spaceship.Api.Dev (runDevApi)
        '';
        test = "rundevApi";
        env.DEV_PORT = "8000";
      };
    };
  };
}
```

This will create an `outputs` attrset that defines packages and checks for all Cabal packages, once with the specified
`compiler` and the `overrides`, and once for each of the compiler versions 8.8.4, 8.10.7 and 9.0.1 with the
`compatOverrides`.

Additionally, `devShell`'s environment contains the project's dependencies plus `cabal`, `ghcid` and
`haskell-language-server`.

The `overrides` are specified using a declarative DSL, for which a set of combinators is provided.

## hpack

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

## ghcid

The `ghcid.commands` attrset is translated into flake apps that run a haskell function in `ghcid` that can be started
with:

```
nix run .#dev-api
```

## haskell-language-server

HLS can be started with:

```
nix develop -c haskell-language-server
```

## hasktags

To generate `ctags` for all dependencies and project packages:

```
nix run .#tags [<tags-file>]
```

`tags-file` defaults to `.tags`.

## cabal upload

To upload package candidates or publish to Hackage:

```
nix run .#candidates
nix run .#release
```

If the arg `versionFile` is given, the script will substitute the `version:` line in that `hpack` file after asking for
the next version.


## Build

To build a package:

```
nix build .#spaceship-api
```

[thax]: https://github.com/tek/thax
