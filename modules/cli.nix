{config, lib, util, ...}:
with lib;
let

  ghcModule = import ./ghc.nix { global = config; inherit util; };

in {

    options.internal.hixCli = with types; {

      ghc = mkOption {
        description = mdDoc "The GHC config used for the Hix CLI, defaulting to the dev GHC without overrides.";
        type = submodule ghcModule;
      };

      overrides = mkOption {
        type = util.types.cabalOverrides;
        description = mdDoc "The overrides used for the CLI client.";
      };

      package = mkOption {
        description =
          "The package for the Hix CLI, defaulting to the local package in the input repository using the dev GHC.";
        type = package;
      };

      dev = mkOption {
        description = mdDoc ''
        Whether to build the CLI from the sources in this checkout rather than from Hackage.
        For testing purposes.
        '';
        type = bool;
        default = false;
      };

      exe = mkOption {
        description = "The executable in the `bin/` directory of [](#opt-hixCli-package).";
        type = path;
        default = "${config.internal.hixCli.package}/bin/hix";
      };

      staticExeUrl = mkOption {
        description = mdDoc "The URL to the Github Actions-built static executable.";
        type = str;
        default = "https://github.com/tek/hix/releases/download/${config.internal.hixVersion}/hix";
      };

  };

  config.internal.hixCli = {

    overrides = {hackage, source, minimal, unbreak, jailbreak, ...}: {
      exon = hackage "1.5.0.0" "07jawnnmpdqfnvmayv64xc4n0j9mbcgdyyqsg3dn3a3z1f4fxnfm";
      flatparse = hackage "0.5.0.1" "0y6axksh2hqp8v58676a7zmwf0in7v6hmyfv8sfdx4x0acq2vjhr";
    } // (
          if config.internal.hixCli.dev
          then { hix = minimal (source.package ../. "hix"); }
          else { hix = jailbreak (minimal (hackage "0.6.7" "1kcab39rwangb4m1viw58ppvf1ps4i75i2dgapzyqklq87f1bmpi")); }
      );

    ghc = {
      name = "hix";
      compiler = "ghc94";
      overrides = mkForce config.internal.hixCli.overrides;
      nixpkgs = config.inputs.nixpkgs;
      nixpkgsOptions = {};
      overlays = [];
    };

    package = config.internal.hixCli.ghc.ghc.hix;

  };

}
