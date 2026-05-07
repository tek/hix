{config, lib, util, ...}:
let
  inherit (lib) types;
  inherit (util) build;

  package-set = build.package-sets config.internal.hixCli.ghc;

in {

    options.internal.hixCli = {

      ghc = lib.mkOption {
        description = "The GHC config used for the Hix CLI.";
        type = types.submodule (import ./package-set.nix { inherit util; });
      };

      overrides = lib.mkOption {
        description = "The overrides used for the CLI package set.";
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

      hash = lib.mkOption {
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

    commit = lib.mkIf (!config.internal.hixRelease) (lib.mkDefault "b79cf30e9275ade0b0303fce3dbb772bb1cb59b0");

    hash = "sha256-xht1kRRnFOslSiDPgahg0AuXjL84bZINYrXCcVaZjtI=";

    overrides = {hackage, source, github, minimal, jailbreak, ...}: let

      conf = config.internal.hixCli;

      githubArgs = {
        owner = "tek";
        repo = "hix";
        rev = conf.commit;
        inherit (conf) hash;
        path =  "packages/hix";
      };

      prodHix = let
        meta = import ../ops/cli-dep.nix;
      in hackage meta.version meta.sha256;

      hix =
        if conf.dev
        then source.package ../. "hix"
        else if conf.commit != null
        then github githubArgs
        else prodHix
        ;

    in { hix = jailbreak (minimal hix); };

    ghc = {
      name = "hix";
      compiler = {
        nixpkgs = {
          source = {
            rev = "a7fc11be66bdfb5cdde611ee5ce381c183da8386";
            hash = "sha256:0h3gvjbrlkvxhbxpy01n603ixv0pjy19n9kf73rdkchdvqcn70j2";
          };
          extends = null;
        };
        source = "ghc912";
        extends = null;
      };
      overrides = lib.mkForce config.internal.hixCli.overrides;
    };

    package = package-set.packages.hix;

  };

}
