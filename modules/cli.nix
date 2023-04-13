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
        description =
          "The executable in the `bin/` directory of [](#opt-hixCli-package).";
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

    overrides = {hackage, source, minimal, unbreak, ...}: {
      exon = unbreak;
      flatparse = hackage "0.4.0.2" "0saxwgwbzijgm9v5w9nx3npl28szpkyz97m4shn8yanxq7gsjnvg";
      hix = let
        src =
          if config.internal.hixCli.dev
          then source.package ../. "hix"
          else hackage "0.4.2" "0q9dzf42xj2zv8ppj0g6pw2fglr971vr72064mvxn14h243zmgpj";
      in minimal src;
    };

    ghc = {
      name = "hix";
      compiler = "ghc92";
      overrides = mkForce config.internal.hixCli.overrides;
      nixpkgs = config.inputs.nixpkgs_internal;
      nixpkgsOptions = {};
      overlays = [];
    };

    package = config.internal.hixCli.ghc.ghc.hix;

  };

}
