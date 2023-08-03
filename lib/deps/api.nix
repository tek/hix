{
  pkgs,
  self,
  super,
}:
let
  inherit (pkgs.lib) flip;

  modifiers = import ./modifiers.nix { inherit pkgs; };
  spec = import ./spec.nix { inherit (pkgs) lib; };
  c2n = import ./cabal2nix.nix { inherit pkgs; };

  inherit (spec) transform transform_ transformDrv drv;
  hl = pkgs.haskell.lib;

  transformers = {
    transformDrv = transform_ "transform-drv";
    jailbreak = transform_ "jailbreak" modifiers.jailbreak;
    configure = flag: transform_ "configure" (flip hl.appendConfigureFlag flag);
    configures = flags: transform_ "configures" (flip hl.appendConfigureFlags flags);
    override = conf: transform_ "override" (flip hl.overrideCabal conf);
    overrideAttrs = f: transform_ "overrideAttrs" (drv: drv.overrideAttrs f);
    buildInputs = inputs: transform_ "buildInputs" (drv: drv.overrideAttrs (old: { buildInputs = old.buildInputs ++ inputs; }));
    minimal = transform_ "minimal" modifiers.minimal;
    profiling = transform_ "profiling" modifiers.profiling;
    noprofiling = transform_ "noprofiling" modifiers.noprofiling;
    unbreak = transform_ "unbreak" modifiers.unbreak;
    fast = transform_ "fast" modifiers.fast;
    notest = transform_ "notest" modifiers.notest;
    nodoc = transform_ "nodoc" modifiers.nodoc;
    bench = transform_ "bench" modifiers.bench;
    nobench = transform_ "nobench" modifiers.nobench;
  };

  reset = drv null;

  noHpack = spec.option "cabal2nix" "Cabal2nix option --no-hpack" "--no-hpack";

  cabalOverrides = spec.option "cabal2nix-overrides" "Cabal2nix overrides";

in transformers // {
  inherit (c2n) hackage source;
  inherit self super pkgs reset transform transform_ transformDrv noHpack cabalOverrides drv;
  inherit (spec) option;
  hsLib = hl;
  inherit (pkgs) system lib;
  ghcName = self.ghc.name;
  ghcVersion = self.ghc.version;
}
