{config, lib, util, ...}:
with lib;
let

  envModule = import ./env.nix { global = config; inherit util; };

  ghcModule = import ./ghc.nix { global = config; inherit util; };

  ghcVersionEnv = compiler: {
    ghc = {
      name = compiler;
      inherit compiler;
      nixpkgs = config.inputs.nixpkgs;
    };
    # TODO this didn't use mkForce before, should that mean that all dep overrides are active in this env?
    internal.overridesInherited = mkForce (util.overridesGlobal [compiler]);
    ifd = mkIf (!config.compat.ifd) false;
  };

  ghcVersionEnvs = genAttrs config.ghcVersions ghcVersionEnv;

in {
  options = with types; {

    envs = mkOption {
      description = mdDoc "All environments for this project.";
      type = attrsOf (submodule envModule);
      default = {};
    };

    ghcVersions = mkOption {
      description = mdDoc ''
      The GHC versions for which to create envs, specified by their attribute names in `pkgs.haskell.packages`.
      '';
      type = listOf str;
      default = ["ghc90" "ghc92" "ghc94" "ghc96"];
    };

    # TODO deprecate
    devGhc = mkOption {
      description = mdDoc "Backwards-compat alias for `envs.dev.ghc`.";
      type = submodule ghcModule;
      readOnly = true;
    };

  };

  config = {
    envs = ghcVersionEnvs // util.managed.envs // {

      dev = {
        ghc = {
          compiler = mkDefault config.compiler;
          overlays = mkDefault [];
          nixpkgs = mkDefault config.inputs.nixpkgs;
          nixpkgsOptions = mkDefault {};
        };
        internal.overridesInherited = (util.overridesGlobal ["dev"]);
        hls.enable = true;
      };

      min = {
        internal.overridesInherited =
          util.concatOverrides [(util.overridesGlobalMin ["dev"]) config.envs.dev.internal.overridesEnv];
        localPackage = api: api.minimal;
      };

      hls = {
        hls.enable = true;
        hls.package = mkDefault config.envs.hls.ghc.ghc.haskell-language-server;
        hide = true;
        hideApps = true;
        ghc.overrides = config.envs.hls.overrides;
        # TODO this didn't use mkForce before, should that mean that all dep overrides are active in this env?
        # ghc.overrides has an explicit redirect here, and the default for that option uses mkDefault, so that should
        # solve the issue. still probably good to use mkForce here, to avoid confusing other consumers of
        # overridesInherited.
        internal.overridesInherited = mkForce [];
        localDeps = false;
      };

    };

    devGhc = mkDefault config.envs.dev.ghc;
  };
}
