{
  pkgs,
  profiling,
}:
{
  self,
  super,
}:
with builtins;
with pkgs.lib;
let
  modifiers = import ./modifiers.nix { inherit pkgs profiling; };
  spec = import ./spec.nix { inherit (pkgs) lib; };

  inherit (spec) transform transform_ set;
  hl = pkgs.haskell.lib;

  option = name: value: spec:
  let
    old = spec.options or {};
  in spec // { options = old // { ${name} = value; }; };

  options = name: default: spec:
  (spec.options or {}).${name} or default;


  transformers = {
    jailbreak = transform_ hl.doJailbreak;
    configure = flag: transform_ (flip hl.appendConfigureFlag flag);
    configures = flags: transform_ (flip hl.appendConfigureFlags flags);
    override = conf: transform_ (flip hl.overrideCabal conf);
    minimal = transform_ modifiers.minimalDrv;
    profiling = transform_ modifiers.profiling;
    unbreak = transform_ modifiers.unbreak;
    fast = transform_ modifiers.fast;
  };

  hackage = ver: sha256: spec.create ({ self, pkg, ... }: {
    drv = modifiers.hackageDrv (self.callHackageDirect { inherit pkg ver sha256; } {});
  });

  cabal2nix = src: spec.create ({ self, final, pkg, ... }: {
    drv = modifiers.globalProfiling (self.callCabal2nixWithOptions pkg src (options "cabal2nix" "" final) {});
  });

  source = rec {
    root = cabal2nix;
    sub = src: path: cabal2nix "${src}/${path}";
    package = src: path: sub src "packages/${path}";
  };

  drv = d: set { drv = d; };

  keep = drv null;

  noHpack = option "cabal2nix" "--no-hpack";

in transformers // {
  inherit hackage source self super pkgs keep transform transform_ option noHpack drv;
  hsLib = hl;
  inherit (pkgs) system lib;
  compilerName = self.ghc.name;
  compilerVersion = self.ghc.version;
}
