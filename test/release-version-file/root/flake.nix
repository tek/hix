{
  description = "hix test project for version file updates";

  inputs.hix.url = "HIX";

  outputs = {hix, ...}: hix.lib._hix_test {

    release.versionFile = "version.nix";

    hackage = {
      repos = {
        "hackage.haskell.org" = {
          publish = false;
          indexState = "2024-11-01T00:00:00Z";
        };
        # Mock repo to satisfy publishable repo requirement
        local = {
          publish = true;
          user = "test";
          password = "test";
          location = "http://localhost:8080";
        };
      };
    };

    managed = {
      enable = true;
      latest.compiler = "ghc910";
    };

    internal.hixCli.dev = true;

    compat.enable = false;

    packages = {
      # Package with Cabal version file
      pkg-cabal = {
        src = ./packages/pkg-cabal;
        versionFile = "packages/pkg-cabal/pkg-cabal.cabal";
        library = {
          enable = true;
        };
      };
      # Package with Nix version file
      pkg-nix = {
        src = ./packages/pkg-nix;
        versionFile = "packages/pkg-nix/version.nix";
        library = {
          enable = true;
        };
      };
      # Package without version file
      pkg-none = {
        src = ./packages/pkg-none;
        library = {
          enable = true;
        };
      };
    };

  };
}
