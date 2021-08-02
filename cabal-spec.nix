{
  pkgs,
  profiling,
}:
{
  self,
  super,
}:
with builtins;
let
  tools = import ./cabal-spec-tools.nix { inherit pkgs profiling; };

  inherit (pkgs.lib.attrsets) filterAttrs foldAttrs isDerivation mapAttrs' nameValuePair;
  inherit (pkgs.lib.lists) foldl;
  inherit (pkgs.lib.debug) traceVal;
  inherit (tools) mkSpec;
  hl = pkgs.haskell.lib;

  transform = hook: spec:
  let wrapped = tools.wrapDrv spec;
  in wrapped // { transforms = ([hook] ++ (tools.transforms wrapped)); };

  option = name: value: spec:
  let
    old = spec.options or {};
  in spec // { options = old // { ${name} = value; }; };

  transformers = {
    jailbreak = transform hl.doJailbreak;
    configure = flag: transform (drv: hl.appendConfigureFlag drv flag);
    configures = flags: transform (drv: hl.appendConfigureFlags drv flags);
    override = conf: transform (drv: hl.overrideCabal drv conf);
    minimal = transform tools.minimalDrv;
  };

  hackage = ver: sha256: mkSpec "hackage" { inherit ver sha256; };

  source = rec {
    root = src: mkSpec "root" { inherit src; };
    sub = src: path: mkSpec "sub" { inherit src path; };
    package = src: path: sub src "packages/${path}";
    output = input: mkSpec "output" { inherit input; };
  };

  conditional = condition: mkSpec "conditional" { inherit condition; };

  keep = mkSpec "keep" {};

  only = target: spec: conditional (_: version: if version == target then spec else keep);

  versions = vs: conditional (_: version: vs.${version} or keep);

  noHpack = option "cabal2nix" "--no-hpack";
in transformers // {
  inherit (tools) unbreak minimalDrv minimalProf drv;
  inherit source hackage conditional only versions self super pkgs keep transform option noHpack;
  hsLib = hl;
  inherit (pkgs) system;
}
