{config, lib, util, ...}:
with lib;
let

  envModule = import ./env.nix { global = config; inherit util; };

  ghcModule = import ./ghc.nix { global = config; inherit util; };

  ghcVersionEnv = compiler: {
    ghc = {
      name = compiler;
      inherit compiler;
      nixpkgs = config.input.ghcNixpkgs.${compiler} or config.inputs.nixpkgs;
    };
    internal.overridesInherited = util.overridesGlobal [compiler];
  };

  ghcVersionEnvs = genAttrs config.ghcVersions ghcVersionEnv;

in {
  options = with types; {

    envs = mkOption {
      description = "";
      type = attrsOf (submodule envModule);
      default = {};
    };

    defaultEnv = mkOption {
      description = "";
      type = submodule envModule;
    };

    ghcVersions = mkOption {
      description = "The GHC versions for which to create envs, specified by their attribute names in `pkgs.haskell.packages`.";
      type = listOf str;
      default = ["ghc8107" "ghc902" "ghc925" "ghc943"];
    };

    devGhc = mkOption {
      description = "Backwards-compat alias for `envs.dev.ghc`.";
      type = submodule ghcModule;
      readOnly = true;
    };

  };

  config = {
    envs = ghcVersionEnvs // {

      dev = {
        ghc.name = "dev";
        internal.overridesInherited = util.overridesGlobal ["dev"];
      };

      min = {
        ghc = {
          name = "min";
          compiler = config.envs.dev.ghc.compiler;
          nixpkgs = config.envs.dev.ghc.nixpkgs;
          nixpkgsOptions = config.envs.dev.ghc.nixpkgsOptions;
          overlays = config.envs.dev.ghc.overlays;
        };
        internal.overridesInherited =
          util.concatOverrides [(util.overridesGlobalMin ["dev"]) config.envs.dev.overrides];
      };

    };

    defaultEnv = config.envs.dev;

    devGhc = mkDefault config.envs.dev.ghc;
  };
}
