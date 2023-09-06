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

You can convert an existing project with Cabal files by executing this command in the project root:

```
nix run 'github:tek/hix?ref=0.6.2#bootstrap'
```

You can create a new project in the current directory:

```
nix run 'github:tek/hix?ref=0.6.2#new' -- --name 'project-name' --author 'Your Name'
```

The manual process consists of first adding Hix to your Haskell project flake by specifying the input:

```nix
{
  inputs.hix.url = "github:tek/hix?ref=0.6.2";
}
```

Then configure your project with NixOS module options:

```nix
{
  description = "Example";
  inputs.hix.url = "github:tek/hix?ref=0.6.2";
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
