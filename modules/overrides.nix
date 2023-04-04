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
      description = mdDoc "The overrides exposed from the flake for integration in downstream projects.";
      type = unspecified;
      default = {
        local = toList config.internal.overridesLocal;
        localMin = toList config.internal.overridesLocalMin;
        all = toList config.overrides;
        dev = toList config.envs.dev.overrides;
      } // genAttrs config.ghcVersions (v: toList config.envs.${v}.overrides);
    };

    internal = let

      overridesDeps =
        map (o: removeAttrs o.overrides ["local" "localMin"]) config.deps ++
        map (o: o.overrides) config.depsFull;

    in {

      overridesLocal = mkOption {
        description = "";
        type = util.types.cabalOverrides;
        default = import ../deps/local.nix {
          inherit config lib;
          inherit (config) localPackage;
        };
      };

      overridesLocalMin = mkOption {
        description = "";
        type = util.types.cabalOverrides;
        default = import ../deps/local.nix {
          inherit config lib;
          localPackage = api@{ fast, ... }: p: fast (config.localPackage api p);
        };
      };

      overridesDeps = mkOption {
        description = "";
        type = attrsOf util.types.cabalOverrides;
        default = util.mergeOverrides overridesDeps;
      };

    };

  };
}
