{config, lib, util, ...}:
with lib;
{
  options = with types; {

    overrides = mkOption {
      description = mdDoc ''
        Cabal package specifications and overrides injected into GHC package sets.
        Each override spec is a function that takes a set of combinators and resources like nixpkgs and should return an
        attrset containing either derivations or a transformation built from those combinators.

        The combinators are described in [](#overrides-combinators).
      '';
      type = util.types.cabalOverrides;
      example = literalExpression ''
      {hackage, fast, jailbreak, ...}: {
        aeson = fast (hackage "2.0.0.0" "sha54321");
        http-client = unbreak;
      };
      '';
      default = [];
    };

    exportedOverrides = mkOption {
      description = mdDoc ''
      These overrides are exposed from the flake for integration in downstream projects via the options
      [](#opt-general-deps) and [](#opt-general-depsFull).

      This is an attrset whose keys indicate where to put the overrides in the dependent project – each version env and
      the `dev` env has their own, while the `all` key is applied globally.
      The special keys `local` and `localMin` contain the local packages and their minimal build variants, respectively.
      Local packages are only propagated when [](#opt-general-depsFull) is used.
      '';
      type = unspecified;
      default = {
        local = util.overridesDeps "local" ++ toList config.envs.dev.internal.overridesLocal;
        localMin = util.overridesDeps "localMin" ++ toList config.envs.min.internal.overridesLocal;
        all = util.overridesDeps "all" ++ toList config.overrides;
        dev = util.overridesDeps "dev" ++ toList config.envs.dev.overrides;
      } // genAttrs config.ghcVersions (v: util.overridesDeps v ++ toList config.envs.${v}.overrides);
    };

    inheritSystemDependentOverrides = mkOption {
      description = mdDoc ''
      Overrides can be exported without a dependency on a system, such that a dependent could use them even if the
      dependency wasn't declared for the desired system.
      However, if [](#opt-general-ifd) is `false` in the dependency, the local packages will need the `system` option,
      and therefore need to be imported from `legacyPackages.<system>.overrides`.
      '';
      type = bool;
      default = true;
    };

    internal = let

      getOverrides = o:
        if config.inheritSystemDependentOverrides
        then o.legacyPackages.${config.system}.overrides
        else o.overrides
        ;

      overridesDeps =
        map (o: removeAttrs (getOverrides o) ["local" "localMin"]) config.deps ++
        map getOverrides config.depsFull;

    in {

      overridesDeps = mkOption {
        description = mdDoc ''
        The overrides inherited from dependency flakes via [](#opt-general-deps) and [](#opt-general-depsFull).
        '';
        type = attrsOf util.types.cabalOverrides;
        default = util.mergeOverrides overridesDeps;
      };

    };

  };
}
