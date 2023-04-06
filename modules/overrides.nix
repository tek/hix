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
        local = toList config.envs.dev.internal.overridesLocal;
        localMin = toList config.envs.min.internal.overridesLocal;
        all = toList config.overrides ++ util.overridesDeps "local";
        dev = toList config.envs.dev.overrides ++ util.overridesDeps "dev";
      } // genAttrs config.ghcVersions (v: toList config.envs.${v}.overrides ++ util.overridesDeps v);
    };

    internal = let

      overridesDeps =
        map (o: removeAttrs o.overrides ["local" "localMin"]) config.deps ++
        map (o: o.overrides) config.depsFull;

    in {

      overridesDeps = mkOption {
        description = "";
        type = attrsOf util.types.cabalOverrides;
        default = util.mergeOverrides overridesDeps;
      };

    };

  };
}
