{pkgs}: let

  inherit (pkgs) lib;

  spec = import ./spec.nix { inherit lib; };
  util = import ../default.nix { inherit lib; };

  inherit (spec) decl;

  option = name: options: options.${name} or [];

  pregenCabal2nixDrv = drv:
  builtins.readFile "${drv.passthru.cabal2nixDeriver}/default.nix";

  # TODO check if this chooses the expected override, i.e. for `revision 2 h1 (revision 1 h2 (super.aeson))` it should
  # pick 2.
  hackageRev = options: let
    values = option "revision" options;
  in if values == [] then { revision = null; sha256 = null; } else lib.head values;

  hackageDrv = meta: {self, pkg, options, ...}: let

    hackageArgs = { inherit (meta) ver sha256; inherit pkg; rev = hackageRev options; };

    c2nArgs = util.catSets (option "cabal2nix-overrides" options);

  in self.callHackageDirect hackageArgs c2nArgs;

  srcHackage = meta: pkg: let
    pkgver = "${pkg}-${meta.ver}";
  in pkgs.fetchzip {
    url = "mirror://hackage/${pkgver}/${pkgver}.tar.gz";
    inherit (meta) sha256;
  };

  pregenHackage = meta: args:
  pregenCabal2nixDrv (hackageDrv meta args);

  hackage = ver: sha256:
  spec.pregen srcHackage pregenHackage (decl "hackage" "Hackage: ${ver}" { inherit ver sha256; } hackageDrv);

  srcHackageAt = meta: pkg: let
    pkgver = "${pkg}-${meta.ver}";
  in pkgs.fetchzip {
    url = "${meta.url}/package/${pkgver}/${pkgver}.tar.gz";
    inherit (meta) sha256;
  };

  hackageAtDrv = meta: {self, pkg, options, pkgs, ...}:
    let
      c2nArgs = util.catSets (option "cabal2nix-overrides" options);

      firstRevision = self.callCabal2nix pkg (srcHackageAt meta pkg) c2nArgs;

      rev = hackageRev options;
    in
    pkgs.haskell.lib.compose.overrideCabal (_: { revision = rev.revision; editedCabalFile = rev.sha256; }) firstRevision;

  pregenHackageAt = meta: args:
  pregenCabal2nixDrv (hackageAtDrv meta args);

  # TODO add a test with local hackage for this
  hackageAt = url: ver: sha256: let
    d = decl "hackage-at" "Hackage at ${url}: ${ver}" { inherit url ver sha256; } hackageAtDrv;
  in spec.pregen srcHackageAt pregenHackageAt d;

  cabal2nixDrv = {src}: {self, pkg, options, ...}:
  self.callCabal2nixWithOptions pkg src (util.unwords options.cabal2nix or []) (util.catSets options.cabal2nix-overrides or []);

  srcCabal2nix = meta: pkg: meta.src;

  pregenCabal2nix = meta: args:
  pregenCabal2nixDrv (cabal2nixDrv meta args);

  cabal2nix = src:
  spec.pregen srcCabal2nix pregenCabal2nix (decl "cabal2nix" "Cabal2nix derivation from ${src}" { inherit src; } cabal2nixDrv);

  source = {
    root = cabal2nix;
    sub = src: path: cabal2nix "${src}/${path}";
    package = src: path: cabal2nix "${src}/packages/${path}";
  };

  local = d: decl "local" "Local package" { drv = d; } (meta: _: meta.drv);

in {
  inherit hackage hackageAt source local;
}
