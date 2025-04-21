{config, lib, util, ...}:
let
  inherit (lib) types;

  ghcModule = import ./ghc.nix { global = config; inherit util; };

  cliNixpkgs = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/b2243f41e860ac85c0b446eadc6930359b294e79.tar.gz";
    sha256 = "0bhibarcx56j1szd40ygv1nm78kap3yr4s24p5cv1kdiy4hsb21k";
  };

in {

    options.internal.hixCli = {

      ghc = lib.mkOption {
        description = "The GHC config used for the Hix CLI, defaulting to the dev GHC without overrides.";
        type = types.submodule ghcModule;
      };

      overrides = lib.mkOption {
        description = "The overrides used for the CLI client.";
        type = util.types.cabalOverrides;
      };

      package = lib.mkOption {
        description = ''
        The package for the Hix CLI, defaulting to the local package in the input repository using the dev GHC.
        '';
        type = types.package;
      };

      commit = lib.mkOption {
        description = ''
        The commit sha of the Hix Github repo from which the package should be built.
        If this is `null`, the default package is used.
        '';
        type = types.nullOr types.str;
        default = null;
      };

      sha256 = lib.mkOption {
        description = ''
        If `commit` is configured, this is the corresponding source hash.
        Initially the empty string, you can add the value after the first build attempt by copying it from the error
        message.
        '';
        type = types.str;
        default = "";
      };

      dev = lib.mkOption {
        description = ''
        Whether to build the CLI from the sources in the Hix input rather than from Hackage.
        For testing purposes.
        '';
        type = types.bool;
        default = false;
      };

      exe = lib.mkOption {
        description = "The executable in the `bin/` directory of [](#opt-hixCli-package).";
        type = types.path;
        default = "${config.internal.hixCli.package}/bin/hix";
      };

      staticExeUrl = lib.mkOption {
        description = "The URL to the Github Actions-built static executable.";
        type = types.str;
        default = "https://github.com/tek/hix/releases/download/${config.internal.hixVersion}/hix";
      };

  };

  config.internal.hixCli = {

    overrides = {hackage, source, minimal, jailbreak, super, ...}: let

      conf = config.internal.hixCli;

      githubSrc = builtins.fetchTarball {
        url = "https://github.com/tek/hix/archive/${conf.commit}.tar.gz";
        inherit (conf) sha256;
      };

      devHix = source.package (if conf.commit == null then ../. else githubSrc) "hix";

      prodHix = let
        meta = import ../ops/cli-dep.nix;
      in jailbreak (hackage meta.version meta.sha256);

      useDev = conf.commit != null || conf.dev;

      hix = if useDev then devHix else prodHix;

    in {
      hix = minimal hix;
      cabal-install = super.cabal-install.overrideScope (cself: csuper: { semaphore-compat = null; });
    } // lib.optionalAttrs (!useDev) {
      exon = hackage "1.6.0.1" "0wnjywsxsmfqhyymzxlk8zzc5k4jr15y8rgl3lcdw48jl80i6ix9";
    };

    ghc = {
      name = "hix";
      compiler = "ghc98";
      overrides = lib.mkForce config.internal.hixCli.overrides;
      nixpkgs = cliNixpkgs;
      nixpkgsOptions = {};
      overlays = [];
    };

    package = config.internal.hixCli.ghc.ghc.hix;

  };

}
