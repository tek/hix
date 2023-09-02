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
      exon = unbreak;
      flatparse = hackage "0.4.0.2" "0saxwgwbzijgm9v5w9nx3npl28szpkyz97m4shn8yanxq7gsjnvg";
    } // (
          if config.internal.hixCli.dev
          then { hix = minimal (source.package ../. "hix"); }
          else { hix = jailbreak (minimal (hackage "0.5.8" "1k6q65vwvi250vlfvhk736s4scp46gmr95jva4ksz2rmradz3sxg")); }
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
