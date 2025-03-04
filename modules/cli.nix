{config, lib, util, ...}:
with lib;
let

  ghcModule = import ./ghc.nix { global = config; inherit util; };

  cliNixpkgs = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/e913ae340076bbb73d9f4d3d065c2bca7caafb16.tar.gz";
    sha256 = "07qjibn85hc6p8c7lwg00fwpyqjlmaxncz9wa2l6qpy1hsk51k8f";
  };

in {

    options.internal.hixCli = with types; {

      ghc = mkOption {
        description = "The GHC config used for the Hix CLI, defaulting to the dev GHC without overrides.";
        type = submodule ghcModule;
      };

      overrides = mkOption {
        description = "The overrides used for the CLI client.";
        type = util.types.cabalOverrides;
      };

      package = mkOption {
        description = ''
        The package for the Hix CLI, defaulting to the local package in the input repository using the dev GHC.
        '';
        type = package;
      };

      dev = mkOption {
        description = ''
        Whether to build the CLI from the sources in the Hix input rather than from Hackage.
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
        description = "The URL to the Github Actions-built static executable.";
        type = str;
        default = "https://github.com/tek/hix/releases/download/${config.internal.hixVersion}/hix";
      };

  };

  config.internal.hixCli = {

    # TODO This should use a fixed nixpkgs – when users override nixpkgs with a commit that's across some boundary of
    # breaking boot library changes (like semaphore-compat in 9.10), hix will not be buildable.
    overrides = {hackage, source, minimal, jailbreak, ...}: let

      devSrc = source.package ../. "hix";

      prodSrc = let
        meta = import ../ops/cli-dep.nix;
      in jailbreak (hackage meta.version meta.sha256);

      hix = if config.internal.hixCli.dev then devSrc else prodSrc;

    in {
      hix = minimal hix;
    } // optionalAttrs (!config.internal.hixCli.dev) {
      exon = hackage "1.6.0.1" "0wnjywsxsmfqhyymzxlk8zzc5k4jr15y8rgl3lcdw48jl80i6ix9";
    };

    ghc = {
      name = "hix";
      compiler = "ghc98";
      overrides = mkForce config.internal.hixCli.overrides;
      nixpkgs = cliNixpkgs;
      nixpkgsOptions = {};
      overlays = [];
    };

    package = config.internal.hixCli.ghc.ghc.hix;

  };

}
