{config, lib, util, ...}:
with lib;
let

  envModule = import ./env.nix { global = config; inherit util; };

  ghcVersionEnv = compiler: {
    ghc = {
      inherit compiler;
      overrideKeys = ["local" "all" "compat" compiler];
      nixpkgs = config.input.ghcNixpkgs.${compiler} or config.inputs.nixpkgs;
    };
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

  };

  config = {
    envs = ghcVersionEnvs // { dev = config.defaultEnv; };

    defaultEnv = {
      ghcid = mkDefault true;
      hls = mkDefault true;
    };
  };
}
