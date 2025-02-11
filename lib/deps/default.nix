{config ? {}}:
{pkgs}:
let
  inherit (pkgs) lib;

  spec = import ./spec.nix { inherit (pkgs) lib; };
  mkApi = import ./api.nix { inherit config pkgs; };

  # This reverses the list of overrides from different sources, because the rightmost source should have the highest
  # precedence and the DSL composes combinators right-to-left.
  normalize = overrides: self: super: let
    api = mkApi { inherit self super; };
  in lib.zipAttrsWith (_: lib.concatLists) (lib.reverseList (map (o: lib.mapAttrs (_: spec.listOC) (o api)) overrides));

  compile = overrides: self: super:
  lib.mapAttrs (_: spec.compile) (normalize overrides self super);

  reifySpec = self: super: pkg: spec.reify { inherit pkgs self super pkg; };

  # Directly reify the override combinators, without using pregenerated derivations.
  reify = overrides: self: super:
  lib.mapAttrs (reifySpec self super) (normalize overrides self super);

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
  inherit normalize compile reify replace;
}
