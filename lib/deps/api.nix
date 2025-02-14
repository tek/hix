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
  inherit (lib) flip;

  modifiers = import ./modifiers.nix { inherit pkgs; };
  spec = import ./spec.nix { inherit lib; };
  c2n = import ./cabal2nix.nix { inherit pkgs; };

  inherit (spec) transform transform_ transformDrv drv;
  hl = pkgs.haskell.lib;

  transformers = {
    transformDrv = transform_ "transform-drv";
    jailbreak = transform_ "jailbreak" modifiers.jailbreak;
    # TODO why does this use `flip`? The functions appear to take the flag arg first
    configure = flag: transform_ "configure" (flip hl.appendConfigureFlag flag);
    configures = flags: transform_ "configures" (flip hl.appendConfigureFlags flags);
    enable = flag: transform_ "enable" (flip hl.enableCabalFlag flag);
    disable = flag: transform_ "disable" (flip hl.disableCabalFlag flag);
    ghcOptions = flag: transform_ "ghcOptions" (hl.compose.appendConfigureFlag "--ghc-options=${flag}");
    override = conf: transform_ "override" (flip hl.overrideCabal conf);
    overrideAttrs = f: transform_ "overrideAttrs" (drv: drv.overrideAttrs f);
    buildInputs = inputs: transform_ "buildInputs" (drv:
      drv.overrideAttrs ({buildInputs ? [], ...}: { buildInputs = buildInputs ++ inputs; })
    );
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

  revision = revision: sha256:
  spec.option "revision" "Hackage revision" { revision = toString revision; inherit sha256; };

  unknownHackage = name: ''
  An override refers to the nonexistent Hackage server config '${name}'.
  '';

  hackageLocation = error: name: let
    server = config.hackage.servers.${name} or null;
  in
  if server == null
  then throw (error name)
  else server.location;

  hackageNoConfig = name: throw ''
  Internal error: An override refers to the Hackage server '${name}', but no config was passed to the API.
  '';

  hackageConfGen = error: server:
  if lib.attrByPath ["hackage" "servers"] null config == null
  then hackageNoConfig server
  else c2n.hackageAt (hackageLocation error server)
  ;

  hackageConf = hackageConfGen unknownHackage;

  hackage = let
    conf = lib.attrByPath ["hackage" "servers" "hix-override"] null config;
  in
  if conf == null
  then c2n.hackage
  else c2n.hackageAt conf.location
  ;

in transformers // {
  inherit (c2n) hackageAt source;
  inherit self super pkgs;
  inherit reset transform transform_ transformDrv noHpack cabalOverrides revision drv;
  inherit hackageConfGen hackageConf hackage;
  inherit (spec) option;
  hsLib = hl;
  inherit (pkgs) system lib;
  ghcName = self.ghc.name;
  ghcVersion = self.ghc.version;
}
