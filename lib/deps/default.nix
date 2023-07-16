{ pkgs, }:
with pkgs.lib;
let
  util = import ../../lib/default.nix { inherit (pkgs) lib; };

  spec = import ./spec.nix { inherit (pkgs) lib; };

  # This reverses the list of overrides from different sources, because the rightmost source should have the highest
  # precedence and the DSL composes combinators right-to-left.
  normalize = overrides: self: super: let
    api = import ./api.nix { inherit pkgs self super; };
  in zipAttrsWith (_: concatLists) (reverseList (map (o: mapAttrs (_: spec.listOC) (o api)) overrides));

  compile = overrides: self: super:
  mapAttrs (_: spec.compile) (normalize overrides self super);

  reifySpec = self: super: pkg: spec.reify { inherit pkgs self super pkg; };

  reify = overrides: self: super:
  mapAttrs (reifySpec self super) (normalize overrides self super);

  replaceSrc = src: drv:
  pkgs.haskell.lib.overrideCabal drv (_: { inherit src; });

  noPackage = pkg: desc: ''
  The package '${pkg}' is declared in the overrides as a pregenerated derivation (${desc}),
  but the generated file does not contain an entry for it.
  Please run 'nix run .#gen-overrides'.
  If that doesn't resolve the issue, the override combinator supplying the derivation might be buggy.
  '';

  pregenDecl = fileError: pregen: pkg: decl: let
    stored = pregen.${pkg};

    properSrc = decl.pregen.src decl.meta pkg;

    impl = meta: {self, ...}: replaceSrc properSrc (self.callPackage meta.drv {});

    d = spec.decl "pregen" "Pregen derivation" { drv = stored.drv; } impl;

    checkMeta =
      if stored.meta != decl.meta
      then throw ''
      The stored metadata of the override for '${pkg}' does not match the current override.
      This is most likely because you changed the override since the last run of 'gen-overrides'.
      Please run 'nix run .#gen-overrides' again.

      Stored:
      ${generators.toPretty {} stored.meta}

      Current:
      ${generators.toPretty {} decl.meta}
      ''
      else d.single;

  in
  if hasAttr pkg pregen
  then checkMeta
  else throw (if fileError == null then noPackage pkg decl.desc else fileError);

  replaceDecl = error: self: super: pregen: pkg: comp: let
    canReplace = comp.decl != null && comp.decl.pregen.enable;
    replaced = comp // optionalAttrs canReplace { decl = pregenDecl error pregen pkg comp.decl; };
  in spec.reifyComp { inherit pkgs self super pkg; } replaced;

  replace = error: pregen: overrides: self: super: let
    comp = compile overrides self super;
  in mapAttrs (replaceDecl error self super pregen) comp;

in {
  inherit normalize compile reify replace;
}
