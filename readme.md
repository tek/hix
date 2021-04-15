# About

A set of tools for developing on a Haskell project with Nix build.
Provides out-of-the-box setup for package overrides, `ghcid`, `haskell-language-server`, [thax], and `cabal upload`.

# Basic usage

`flake.nix`:

```nix
{
  description = "A Haskell Library";

  inputs.hix.url = github:tek/hix;
  inputs.polysemy-log.url = github:tek/polysemy-log;

  outputs = { hix, polysemy-log, ... }:
  let
    common = { hackage, jailbreak, ... }: {
      co-log-polysemy = jailbreak (hackage "0.0.1.2" "17bcs8dvrhwfcyklknkqg11gxgxm2jaa7kbm6xx4vm1976abzwss");
      polysemy = hackage "1.5.0.0" "1xl472xqdxnp4ysyqnackpfn6wbx03rlgwmy9907bklrh557il6d";
    };

    main = { hackage, source, ... }: {
      path = hackage "0.8.0" "0isldidz2gypw2pz399g6rn77x9mppd1mvj5h6ify4pj4mpla0pb";
      polysemy-log = source.sub polysemy-log "packages/polysemy-log";
    };

    compat = { hackage, only, ... }: {
      polysemy = only "865" (hackage "1.4.0.0" "04bl0w7z35jh63jpy87sa1rrbgqhwn7c0pxsm5l3ww0pjnswkhjj");
    };

  in hix.flake {
    base = ./.;
    compiler = "ghc8104";
    packages = {
      spaceship-core = "packages/core";
      spaceship-api = "packages/api";
    };
    main = "spaceship-api";
    overrides = [common main];
    compatOverrides = [common compat];
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

This will create an `outputs` attrset that contains packages and checks for all packages, once with the specified
`compiler` and the `overrides`, and once for each of the compiler versions 8.6.5, 8.8.4 and 8.10.4 with the
`compatOverrides`.

Additionally, `devShell`'s environment contains the project's dependencies plus `cabal`, `ghcide` and
`haskell-language-server`.

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

## cabal

To upload package candidates or publish to Hackage:

```
nix run .#candidates
nix run .#release
```

If the arg `versionFile` is given, the script will substitute the `version:` line in that hpack file after asking for
the next version.


## Build

To build a package:

```
nix build .#spaceship-api
```

[thax]: https://github.com/tek/thax
