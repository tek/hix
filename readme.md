# About

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

To learn more, please visit the [documentation page](https://tryp.io/hix/index.html).

# tldr

You can convert an existing Cabal project by executing this command in the project root, using
[FlakeHub](https://flakehub.com/docs) or GitHub:

```
nix run 'https://flakehub.com/f/tek/hix/~0.9.tar.gz#bootstrap'
nix run 'github:tek/hix?ref=0.9.1#bootstrap'
```

You can create a new project in the current or an arbitrary directory:

```
nix run 'https://flakehub.com/f/tek/hix/~0.9.tar.gz#init' -- --author 'Your Name' --name project-name
nix run 'https://flakehub.com/f/tek/hix/~0.9.tar.gz#new' -- --author 'Your Name' path/to/project-name
```

The manual process consists of first adding Hix to your Haskell project flake by specifying the input:

```nix
{
  inputs.hix.url = "https://flakehub.com/f/tek/hix/~0.9.tar.gz";
}
```

Then configure your project with NixOS module options:

```nix
{
  description = "Example";
  inputs.hix.url = "https://flakehub.com/f/tek/hix/~0.9.tar.gz";
  outputs = {hix, ...}: hix {
    packages.parser = {
      src = ./.;
      library = {
        enable = true;
        dependencies = ["aeson ^>= 2.0" "bytestring"];
      };
      executable.enable = true;
      test.enable = true;
    };
  };
}
```

Now generate Cabal files with:

```
nix run .#gen-cabal
```

Build the package with `nix build`, or run the tests in `test/Main.hs` in GHCid:

```
nix run .#ghcid -- -p parser
```
