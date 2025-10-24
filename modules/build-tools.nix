{config, lib, ...}: let

  inherit (lib) types;

  ghc = config.envs.hix-build-tools.toolchain.packages;

in {

  options.build-tools = {

    cabal.package = lib.mkOption {
      description = "A package containing the Cabal executable at `bin/cabal`.";
      type = types.package;
    };

    hpack.package = lib.mkOption {
      description = "A package containing the HPack executable at `bin/hpack`.";
      type = types.package;
    };

    ghcid.package = lib.mkOption {
      description = "A package containing the ghcid executable at `bin/ghcid`.";
      type = types.package;
    };

  };

  config.build-tools = {

    cabal.package = lib.mkDefault ghc.cabal-install;

    hpack.package = lib.mkDefault ghc.hpack;

    ghcid.package = lib.mkDefault ghc.ghcid;

  };

}
