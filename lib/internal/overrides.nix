{util}: let

  inherit (util) config lib internal;

  gen = config.gen-overrides;

  path = "${util.project.base}/${gen.file}";

  noOverridesFile = file: ''
  The option 'gen-overrides.enable' is set, but the file '${file}' doesn't exist.
  Please run 'nix run .#gen-overrides' to create it.
  If it does exist, you probably need to 'git add' it.
  '';

  noOverridesGhc = name: ''
  The pregenerated overrides do not contain an entry for the GHC package set named '${name}'.
  Please run 'nix run .#gen-overrides' again if you changed this GHC, otherwise this might be a bug.
  '';

  readOverrides = name: let

    exists = lib.pathExists path;
    pregen = lib.optionalAttrs exists (import path);
    stored = lib.optionalAttrs exists (pregen.${name} or {});

    error =
      if exists
      then
        if lib.hasAttr name pregen
        then null
        else noOverridesGhc name
      else noOverridesFile gen.file;

  in { inherit error stored; };

  forPackageSet = {
    # Note: This must be the nixpkgs into which the overrides are inserted as an overlay.
    # In particular, it should be the `prev` argument of the overlay function.
    pkgs,
    packageSet,
  }: let

    deps = import ../deps/default.nix { inherit config; } { inherit pkgs; };

    name = packageSet.name;

    allOverrides = packageSet.overrides ++ packageSet.extraOverrides;

    directOverrides = deps.reify allOverrides;

    restoreOverrides = let
      fromFile = readOverrides name;
    in deps.replace fromFile.error name fromFile.stored allOverrides;

  in
    if gen.enable && packageSet.gen-overrides && !internal.managed.state.current.resolving
    then restoreOverrides
    else directOverrides
    ;

in {
  inherit readOverrides forPackageSet;
}
