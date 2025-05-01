{
  pkgs,
  config ? {},
}:
{
  self,
  super,
}:
let
  inherit (pkgs) lib;

  modifiers = import ./modifiers.nix { inherit pkgs; };
  spec = import ./spec.nix { inherit lib; };
  c2n = import ./cabal2nix.nix { inherit pkgs; };

  inherit (spec) transform transform_ drv;
  hsLibC = pkgs.haskell.lib.compose;

  overrideAttrsCabal = f: drv:
  (drv.override (args: args // { mkDerivation = drv: (args.mkDerivation drv).overrideAttrs f; }))
  // { overrideScope = scope: overrideAttrsCabal f (drv.overrideScope scope); };

  ghcOption = opt: "--ghc-options=${opt}";

  concatGhcOptions = opts: lib.concatMapStringsSep " " ghcOption (lib.toList opts);

  transformers = {
    transformDrv = transform_ "transform-drv";
    modify = transform_ "modify";
    id = transform_ "id" lib.id;
    jailbreak = transform_ "jailbreak" modifiers.jailbreak;
    configure = flag: transform_ "configure" (hsLibC.appendConfigureFlag flag);
    configures = flags: transform_ "configures" (hsLibC.appendConfigureFlags flags);
    enable = flag: transform_ "enable" (hsLibC.enableCabalFlag flag);
    disable = flag: transform_ "disable" (hsLibC.disableCabalFlag flag);
    ghcOption = opt: transform_ "ghcOption" (hsLibC.appendConfigureFlag (ghcOption opt));
    ghcOptions = opts: transform_ "ghcOptions" (hsLibC.appendConfigureFlag (concatGhcOptions opts));
    override = conf: transform_ "override" (hsLibC.overrideCabal conf);
    overrideAttrs = f: transform_ "overrideAttrs" (overrideAttrsCabal f);
    buildInputs = inputs: transform_ "buildInputs" (
      hsLibC.overrideCabal ({buildDepends ? [], ...}: { buildDepends = buildDepends ++ inputs; })
    );
    minimal = transform_ "minimal" modifiers.minimal;
    profiling = transform_ "profiling" modifiers.profiling;
    noprofiling = transform_ "noprofiling" modifiers.noprofiling;
    unbreak = transform_ "unbreak" modifiers.unbreak;
    fast = transform_ "fast" modifiers.fast;
    force = transform_ "force" modifiers.force;
    force' = transform_ "force'" modifiers.force';
    notest = transform_ "notest" modifiers.notest;
    nodoc = transform_ "nodoc" modifiers.nodoc;
    bench = transform_ "bench" modifiers.bench;
    nobench = transform_ "nobench" modifiers.nobench;
  };

  reset = drv null;

  noHpack = spec.option "cabal2nix" "Cabal2nix option --no-hpack" "--no-hpack";

  cabal2nixOverrides = spec.option "cabal2nix-overrides" "Cabal2nix overrides";

  cabalOverrides = cabal2nixOverrides;

  cabal2nixArgs = spec.option "cabal2nix" "Pass a CLI flag to cabal2nix";

  revision = revision: sha256:
  spec.option "revision" "Hackage revision" { revision = toString revision; inherit sha256; };

  unknownHackage = name: ''
  An override refers to the nonexistent Hackage repo config '${name}'.
  '';

  hackageLocation = error: name: let
    repo = config.hackage.repos.${name} or null;
  in
  if repo == null
  then throw (error name)
  else repo.location;

  hackageNoConfig = name: throw ''
  Internal error: An override refers to the Hackage repo '${name}', but no config was passed to the API.
  '';

  hackageConfGen = error: repo:
  if lib.attrByPath ["hackage" "repos"] null config == null
  then hackageNoConfig repo
  else c2n.hackageAt (hackageLocation error repo)
  ;

  hackageConf = hackageConfGen unknownHackage;

  hackage = let
    conf = lib.attrByPath ["hackage" "repos" "hix-override"] null config;
  in
  if conf == null
  then c2n.hackage
  else c2n.hackageAt conf.location
  ;

in transformers // {
  inherit (c2n) hackageAt source;
  inherit self super pkgs;
  inherit reset transform transform_ noHpack cabalOverrides cabal2nixOverrides cabal2nixArgs revision drv;
  inherit hackageConfGen hackageConf hackage;
  inherit (spec) option;
  hsLib = pkgs.haskell.lib;
  inherit hsLibC;
  inherit (pkgs) system lib;
  ghcName = self.ghc.name;
  ghcVersion = self.ghc.version;
}
