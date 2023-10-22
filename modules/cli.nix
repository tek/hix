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
        description = mdDoc "The overrides used for the CLI client.";
        type = util.types.cabalOverrides;
      };

      package = mkOption {
        description = mdDoc ''
        The package for the Hix CLI, defaulting to the local package in the input repository using the dev GHC.
        '';
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
        description = mdDoc "The executable in the `bin/` directory of [](#opt-hixCli-package).";
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

    overrides = {hackage, source, minimal, jailbreak, ...}: let

      devSrc = source.package ../. "hix";

      prodSrc = let
        meta = import ../ops/cli-dep.nix;
      in jailbreak (hackage meta.version meta.sha256);

      hix = if config.internal.hixCli.dev then devSrc else prodSrc ;

    in {
      hix = minimal hix;
      exon = hackage "1.6.0.1" "0wnjywsxsmfqhyymzxlk8zzc5k4jr15y8rgl3lcdw48jl80i6ix9";
    };

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
