{
  global,
  ghc,
}:
final: prev:
with prev.lib;
let

  deps = import ./deps/default.nix { pkgs = prev; };

  gen = global.gen-overrides;

  reified = deps.reify ghc.overrides;

  path = "${global.base}/${gen.file}";

  noOverridesFile = file: ''
  The option 'gen-overrides.enable' is set, but the file '${file}' doesn't exist.
  Please run 'nix run .#gen-overrides' to create it.
  If it does exist, you probably need to 'git add' it.
  '';

  noOverridesGhc = ''
  The pregenerated overrides do not contain an entry for the GHC set named '${ghc.name}'.
  Please run 'nix run .#gen-overrides' again if you changed this GHC, otherwise this might be a bug.
  '';

  readOverrides = self: super: let
    exists = pathExists path;
    pregen = optionalAttrs exists (import path);
    stored = optionalAttrs exists (pregen.${ghc.name} or {});
    error = if exists then if hasAttr ghc.name pregen then null else noOverridesGhc else noOverridesFile gen.file;
  in deps.replace error stored ghc.overrides self super;

  computeOverrides =
    if gen.enable && ghc.gen-overrides
    then readOverrides
    else reified;

  packages = prev.haskell.packages.${ghc.compiler}.override { overrides = computeOverrides; };

in {
  hixPackages = packages // { hix-nixpkgs-rev = ghc.nixpkgs.rev; hix-name = ghc.name; };
}
