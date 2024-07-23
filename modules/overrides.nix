{config, lib, util, ...}:
with lib;
{
  options = with types; {

    overrides = mkOption {
      description = ''
      Cabal package specifications and overrides injected into GHC package sets.
      Each override spec is a function that takes a set of combinators and resources like nixpkgs and should return an
      attrset containing either derivations or a transformation built from those combinators.

      The combinators are described in [](#overrides-combinators).
      '';
      type = util.types.cabalOverridesVia "global";
      example = literalExpression ''
      {hackage, fast, jailbreak, ...}: {
        aeson = fast (hackage "2.0.0.0" "sha54321");
        http-client = unbreak;
      };
      '';
      default = [];
    };

    buildInputs = mkOption {
      description = "Additional non-Haskell dependencies provided to all packages and environments.";
      type = either (functionTo (listOf package)) (listOf package);
      default = [];
    };

    exportedOverrides = mkOption {
      description = ''
      These overrides are exposed from the flake for integration in downstream projects via the options
      [](#opt-general-deps) and [](#opt-general-depsFull).

      This is an attrset whose keys indicate where to put the overrides in the dependent project – each version env and
      the `dev` env has their own, while the `all` key is applied globally.
      The special keys `local` and `localMin` contain the local packages and their minimal build variants, respectively.
      Local packages are only propagated when [](#opt-general-depsFull) is used.
      '';
      type = lazyAttrsOf (util.types.cabalOverridesVia (util.withMainNameOr "dependency" id));
      default = {};
    };

    inheritSystemDependentOverrides = mkOption {
      description = ''
      Overrides can be exported without a dependency on a system, such that a dependent could use them even if the
      dependency wasn't declared for the desired system.
      However, if [](#opt-general-ifd) is `false` in the dependency, the local packages will need the `system` option,
      and therefore need to be imported from `legacyPackages.<system>.overrides`.
      '';
      type = bool;
      default = true;
    };

    gen-overrides = {

      enable = mkOption {
        description = ''
        The flake app `.#gen-overrides` collects all cabal2nix-based derivations from the [overrides](#ghc) that would
        require IFD when computed on the fly.

        Setting this flag instructs Hix to read the generated derivations when building, and to abort the build when
        they are missing or outdated.
        '';
        type = bool;
        default = false;
      };

      file = mkOption {
        description = "The relative path of the file in which the overrides are stored.";
        type = str;
        default = "ops/overrides.nix";
      };

      gitAdd = mkOption {
        description = ''
        Git-add [the overrides file](#opt-general-gen-overrides.file) after the first run.
        Since nix ignores untracked files in flakes, the next command would complain if the file wasn't added.
        '';
        type = bool;
        default = true;
      };

    };

    internal = let

      getOverrides = o: let
        sys = o.legacyPackages.${config.system};
      in
        if config.inheritSystemDependentOverrides && hasAttr config.system o.legacyPackages && sys ? overrides
        then sys.overrides
        else o.overrides
        ;

      overridesDeps =
        map (o: removeAttrs (getOverrides o) ["local" "localMin"]) config.deps ++
        map getOverrides config.depsFull;

    in {

      overridesDeps = mkOption {
        description = ''
        The overrides inherited from dependency flakes via [](#opt-general-deps) and [](#opt-general-depsFull).
        '';
        type = attrsOf util.types.cabalOverrides;
        default = util.mergeOverrides overridesDeps;
      };

    };

  };

  config.exportedOverrides = mkDefault (
    {
      local = util.overridesDeps "local" ++ toList config.envs.dev.internal.overridesLocal;
      localMin = util.overridesDeps "localMin" ++ toList config.envs.min.internal.overridesLocal;
      all = util.overridesDeps "all" ++ toList config.overrides;
      dev = util.overridesDeps "dev" ++ toList config.envs.dev.internal.overridesEnv;
    }
    //
    genAttrs config.ghcVersions (v: util.overridesDeps v ++ toList config.envs.${v}.overrides)
  );

}
