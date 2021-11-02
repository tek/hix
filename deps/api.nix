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

  transformers = {
    jailbreak = transform_ hl.doJailbreak;
    configure = flag: transform_ (flip hl.appendConfigureFlag flag);
    configures = flags: transform_ (flip hl.appendConfigureFlags flags);
    override = conf: transform_ (flip hl.overrideCabal conf);
    minimal = transform_ modifiers.minimalDrv;
    minimalProf = transform_ modifiers.minimalProf;
    profiling = transform_ modifiers.profiling;
    unbreak = transform_ modifiers.unbreak;
  };

  hackage = ver: sha256: spec.create ({ self, pkg, ... }: {
    drv = modifiers.hackageDrv (self.callHackageDirect { inherit pkg ver sha256; } {});
  });

  source = rec {
    root = src: mkSpec "root" { inherit src; };
    sub = src: path: mkSpec "sub" { inherit src path; };
    package = src: path: sub src "packages/${path}";
    output = input: mkSpec "output" { inherit input; };
  };

  drv = d: set { drv = d; };

  keep = drv null;

  noHpack = option "cabal2nix" "--no-hpack";

in transformers // {
  inherit hackage self super pkgs keep transform transform_ option noHpack drv;
  hsLib = hl;
  inherit (pkgs) system lib;
  compilerName = self.ghc.name;
  compilerVersion = self.ghc.version;
}
