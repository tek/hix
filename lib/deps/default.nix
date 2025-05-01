{config ? {}}:
{pkgs}:
let
  inherit (pkgs) lib;

  spec = import ./spec.nix { inherit (pkgs) lib; };
  mkApi = import ./api.nix { inherit config pkgs; };

  # A mock derivation used to exfiltrate the function `mkDerivation` that is manipulated by `overrideCabal`.
  # The initial value is taken from the `super` set.
  mkDerivationSpec = super:
    spec.drv (lib.makeOverridable ({mkDerivation}: { inherit mkDerivation; }) { inherit (super) mkDerivation; });

  # Transform a list of OCs into a derivation for `pkg` using `self` and `super` as the active GHC package set.
  # If the OCs do not include a `decl` spec, transformations are applied to `super.${pkg}`.
  reifySpecs = self: super: pkg: spec.reify { inherit pkgs self super pkg; };

  # This reverses the list of overrides from different sources, because the rightmost source should have the highest
  # precedence and the DSL composes combinators right-to-left.
  normalize = overrides: self: super: let
    api = mkApi { inherit self super; };
    flat = map (o: lib.mapAttrs (_: spec.listOC) (o api)) overrides;
  in lib.zipAttrsWith (_: lib.concatLists) (lib.reverseList flat);

  # Apply the function `f` to each override after normalizing it.
  # The override for `__all` is applied to `mkDerivation` and removed from the resulting set.
  apply = f: overrides: self: super: let
    normalized = normalize overrides self super;

    regular = lib.mapAttrs f (builtins.removeAttrs normalized ["__all"]);

    mkDerivation = let
      dummy = spec.listOC (mkDerivationSpec super);
      withOverrides = reifySpecs self super "__all" (normalized.__all ++ dummy);
    in withOverrides.mkDerivation;

    special =
      if normalized ? __all
      then { inherit mkDerivation; }
      else {};

  in regular // special;

  compile = apply (_: spec.compile);

  # Directly reify the override combinators, without using pregenerated derivations.
  reify = overrides: self: super:
  apply (reifySpecs self super) overrides self super;

  noPackage = ghcName: pkg: desc: ''
  The package '${pkg}' is declared in the overrides as a pregenerated derivation (${desc}),
  but the generated file does not contain an entry for it in the GHC set named '${ghcName}'.
  Please run 'nix run .#gen-overrides'.
  If that doesn't resolve the issue, the override combinator supplying the derivation might be buggy.
  '';

  pregenDecl = fileError: ghcName: pregen: pkg: decl: let
    stored = pregen.${pkg};

    properSrc = decl.pregen.src decl.meta pkg;

    d = spec.decl "pregen" "Pregen derivation for '${pkg}'" { drv = stored.drv; } (decl.pregen.hydrate properSrc);

    checkMeta =
      if stored.meta != decl.meta
      then throw ''
      The stored metadata of the override for '${pkg}' does not match the current override.
      This is most likely because you changed the override since the last run of 'gen-overrides'.
      Please run 'nix run .#gen-overrides' again.

      Stored:
      ${lib.generators.toPretty {} stored.meta}

      Current:
      ${lib.generators.toPretty {} decl.meta}
      ''
      else d.single;

  in
  if lib.hasAttr pkg pregen
  then checkMeta
  else throw (if fileError == null then noPackage ghcName pkg decl.desc else fileError);

  replaceDecl = error: ghcName: self: super: pregen: pkg: comp: let
    canReplace = comp.decl != null && comp.decl.pregen.enable;
    replaced = comp // lib.optionalAttrs canReplace { decl = pregenDecl error ghcName pregen pkg comp.decl; };
  in spec.reifyComp { inherit pkgs self super pkg; } replaced;

  # Use the pregenerated derivations in `pregen`, which were read from the persisted file, in combination with the OC
  # declarations in `overrides`, to produce final derivations with transformations applied.
  replace = error: ghcName: pregen: overrides: self: super: let
    # Directly compiled OCs, containing non-persistable transformations like `jailbreak`.
    comp = compile overrides self super;
  in lib.mapAttrs (replaceDecl error ghcName self super pregen) comp;

in {
  inherit normalize apply compile reify replace;
}
