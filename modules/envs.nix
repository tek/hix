{config, lib, util, ...}:
let

  inherit (lib) types mkDefault;
  inherit (util) build internal;

  envModule = import ./env.nix { global = config; inherit util; };

  ghcVersionEnv = compiler: {
    package-set = {
      name = compiler;
      compiler = {
        extends = lib.mkDefault compiler;
      };
    };
    # TODO this didn't use mkForce before, should that mean that all dep overrides are active in this env?
    internal.overridesInherited = lib.mkForce (util.overridesGlobal [compiler]);
    ifd = lib.mkIf (!config.compat.ifd) false;

    expose = let
      isCompat = config.compat.enable && lib.elem compiler config.compat.versions;
    in {
      packages = mkDefault true;
      apps = mkDefault true;
      checks = mkDefault isCompat;
      scoped = mkDefault true;
      shell = mkDefault true;
      envKeyed = mkDefault false;
    };

  };

  ghcVersionEnvs = lib.genAttrs config.ghcVersions ghcVersionEnv;

  ghcVersionCompilers = lib.genAttrs config.ghcVersions (compiler: { source = compiler; });

in {
  options = {

    envs = lib.mkOption {
      description = "All environments for this project.";
      type = types.attrsOf (types.submodule envModule);
      default = {};
    };

    ghcVersions = lib.mkOption {
      description = ''
      The GHC versions for which to create envs, specified by their attribute names in `pkgs.haskell.packages`.
      '';
      type = types.listOf types.str;
      default = ["ghc94" "ghc96" "ghc98" "ghc910"];
    };

  };

  config = {

    compilers = ghcVersionCompilers;

    envs = ghcVersionEnvs // internal.managed.env.modules // {

      dev = {
        internal.overridesInherited = (util.overridesGlobal ["dev"]);
        hls.enable = lib.mkDefault true;
        expose = {
          packages = mkDefault true;
          apps = mkDefault true;
          checks = mkDefault true;
          scoped = mkDefault true;
          shell = mkDefault true;
          envKeyed = mkDefault false;
        };
      };

      min = {
        packages = mkDefault config.envs.dev.packages;
        internal.overridesInherited =
          util.concatOverrides [(util.overridesGlobalMin ["dev"]) config.envs.dev.internal.overridesEnv];
        localPackage = api: api.minimal;
        expose = {
          envKeyed = mkDefault true;
          scoped = mkDefault true;
        };
      };

      profiled = {
        packages = mkDefault config.envs.dev.packages;
        profiling = true;
        expose = {
          envKeyed = mkDefault true;
          scoped = mkDefault true;
        };
      };

      hls = {
        hls.enable = true;
        hls.package = internal.modules.envDefault build.envs.hls.toolchain.packages.haskell-language-server;
        package-set.overrides = config.envs.hls.overrides;
        localOverrides = false;
        inheritOverrides = false;
        localDeps = false;
        packages = [];
      };

      hix-build-tools = {
        packages = ["hpack" "cabal-install"];
        localDeps = false;
        localOverrides = false;
        globalOverrides = false;
        inheritOverrides = false;
      };

    };

  };

}
