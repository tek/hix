{pkgs}: let

  inherit (pkgs) lib;

  spec = import ./spec.nix { inherit lib; };
  util = import ../default.nix { inherit lib; };

  inherit (spec) decl;

  option = name: options: options.${name} or [];

  optionCat = name: options: util.catSets (option name options);

  optionWords = name: options: util.unwords (option name options);

# ----------------------------------------------------------------------------------------------------------------------

  pregenCabal2nixDrv = drv:
  builtins.readFile "${drv.passthru.cabal2nixDeriver}/default.nix";

  replaceCabal2nixSrc = src: drv:
  pkgs.haskell.lib.overrideCabal drv (_: { inherit src; });

  hackageRev = options: let
    values = option "revision" options;
  in if values == [] then { revision = null; sha256 = null; } else lib.head values;

  setRevision = pkgs: options: let
    rev = hackageRev options;
  in pkgs.haskell.lib.compose.overrideCabal ({passthru ? null, ...}: {
    revision = rev.revision;
    editedCabalFile = rev.sha256;
    passthru = util.fromMaybeNull {} passthru // { inherit (rev) revision; };
  });

  srcHackage = mkBaseUrl: meta: pkg: let
    pkgver = "${pkg}-${meta.ver}";
  in pkgs.fetchzip {
    url = "${mkBaseUrl meta}/${pkgver}/${pkgver}.tar.gz";
    inherit (meta) sha256;
  };

  hydrateCabal2nix = properSrc: meta: {self, options, pkgs, ...}: let
    c2nArgs = optionCat "cabal2nix-overrides" options;
  in setRevision pkgs options (replaceCabal2nixSrc properSrc (self.callPackage meta.drv c2nArgs));

  genHackageDrv = createDrv: meta: args@{self, pkg, options, pkgs, ...}: let

    c2nArgs = optionCat "cabal2nix-overrides" options;

  in setRevision pkgs options (createDrv meta args c2nArgs);

  genHackage = {name, desc, meta, mkBaseUrl, createDrv}: let
    src = srcHackage mkBaseUrl;
    mkDrv = genHackageDrv (meta: args: createDrv (src meta args.pkg) meta args);
    pregen = meta': args: pregenCabal2nixDrv (mkDrv meta' args);
    d = decl name desc meta mkDrv;
  in spec.pregen src pregen hydrateCabal2nix d;

# ----------------------------------------------------------------------------------------------------------------------

  callHackage = meta: {self, pkg, ...}: c2nArgs:
  self.callHackageDirect { inherit (meta) ver sha256; inherit pkg; } c2nArgs;

  hackage = ver: sha256:
  genHackage {
    name = "hackage";
    desc = "Hackage: ${ver}";
    meta = { inherit ver sha256; };
    mkBaseUrl = _: "mirror://hackage";
    createDrv = _: callHackage;
  };

# ----------------------------------------------------------------------------------------------------------------------

  callHackageAt = src: meta: {self, pkg, ...}: c2nArgs:
  self.callCabal2nix pkg src c2nArgs;

  # TODO add a test with local hackage for this
  hackageAt = url: ver: sha256:
  genHackage {
    name = "hackage-at";
    desc = "Hackage at ${url}: ${ver}";
    meta = { inherit url ver sha256; };
    mkBaseUrl = meta: "${meta.url}/package";
    createDrv = callHackageAt;
  };

# ----------------------------------------------------------------------------------------------------------------------

  cabal2nixDrv = {src}: {self, pkg, options, ...}:
  self.callCabal2nixWithOptions pkg src (optionWords "cabal2nix" options) (optionCat "cabal2nix-overrides" options);

  srcCabal2nix = meta: pkg: meta.src;

  pregenCabal2nix = meta: args:
  pregenCabal2nixDrv (cabal2nixDrv meta args);

  cabal2nix = src: let
    d = decl "cabal2nix" "Cabal2nix derivation from ${src}" { inherit src; } cabal2nixDrv;
  in spec.pregen srcCabal2nix pregenCabal2nix hydrateCabal2nix d;

# ----------------------------------------------------------------------------------------------------------------------

  source = {
    root = cabal2nix;
    sub = src: path: cabal2nix "${src}/${path}";
    package = src: path: cabal2nix "${src}/packages/${path}";
  };

  local = d: decl "local" "Local package" { drv = d; } (meta: _: meta.drv);

in {
  inherit hackage hackageAt source local;
}
