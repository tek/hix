{pkgs}: let

  spec = import ./spec.nix { inherit (pkgs) lib; };
  util = import ../default.nix { inherit (pkgs) lib; };

  inherit (spec) decl;

  hackageDrv = meta: {self, pkg, options, ...}:
  self.callHackageDirect { inherit (meta) ver sha256; inherit pkg; } (util.foldAttrs options.cabal2nix-overrides or []);

  srcHackage = meta: pkg: let
    pkgver = "${pkg}-${meta.ver}";
  in pkgs.fetchzip {
    url = "mirror://hackage/${pkgver}/${pkgver}.tar.gz";
    inherit (meta) sha256;
  };

  pregenHackage = meta: args: let
    drv = hackageDrv meta args;
  in builtins.readFile "${drv.passthru.cabal2nixDeriver}/default.nix";

  hackage = ver: sha256:
  spec.pregen srcHackage pregenHackage (decl "hackage" "Hackage: ${ver}" { inherit ver sha256; } hackageDrv);

  cabal2nixDrv = {src}: {self, pkg, options, ...}:
  self.callCabal2nixWithOptions pkg src (util.unwords options.cabal2nix or []) (util.foldAttrs options.cabal2nix-overrides or []);

  srcCabal2nix = meta: pkg: meta.src;

  pregenCabal2nix = meta: args: let
    drv = cabal2nixDrv meta args;
  in builtins.readFile "${drv.passthru.cabal2nixDeriver}/default.nix";

  cabal2nix = src:
  spec.pregen srcCabal2nix pregenCabal2nix (decl "cabal2nix" "Cabal2nix derivation from ${src}" { inherit src; } cabal2nixDrv);

  source = {
    root = cabal2nix;
    sub = src: path: cabal2nix "${src}/${path}";
    package = src: path: cabal2nix "${src}/packages/${path}";
  };

  local = d: decl "local" "Local package" { drv = d; } (meta: _: meta.drv);

in {
  inherit hackage source local;
}
