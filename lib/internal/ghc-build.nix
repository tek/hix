{util}:
{configName, conf, tag}:
{
  pkgs,
  buildPackages,
  pkgsBuildBuild,
  pkgsBuildTarget,
  newScope,
}: let

  inherit (util) lib internal;

  # TODO pass in the entire path of the config and prefix `build.version`
  noVersion = throw ''
  The GHC configuration '${configName}' enables a custom build, but does not specify 'build.version'.
  This is required even if the version isn't used to select the sources.
  '';

  version =
    if conf.version == null
    then throw noVersion
    else conf.version
    ;

  fetchArgs = {
    inherit (conf) version rev postFetch;
    sha256 = conf.hash;
  } // util.pickNonNullAttr "url" conf;

  sourceArg = util.pickNonNullAttrAs "ghcSrc" "src" conf;

  flavourArg = util.pickNonNullAttrAs "ghcFlavour" "flavour" conf;

  optionPrefix = "compilers.${configName}";

  noHash = ''
  The configuration for the custom build of the GHC named '${configName}' does not specify a hash for the source tree.
  If this is the first evaluation, copy the hash that Nix will print below and assign it to the option:
    ${optionPrefix}.source.build.hash
  '';

  warnHash =
    internal.warn.warnEval (conf.hash == "" && conf.src == null) "compiler.build.source-hash" noHash;

  haskellModules = "${pkgs.path}/pkgs/development/haskell-modules";

  callPackage = newScope {
    haskellLib = pkgs.haskell.lib.compose;
    overrides = pkgs.haskell.packageOverrides;
  };

  # --------------------------------------------------------------------------------------------------------------------
  # Compiler config

  actualMinor = lib.versions.minor version;

  actualMinorInt = lib.toInt actualMinor;

  configMinor =
    if lib.trivial.mod actualMinorInt 2 == 0
    then actualMinor
    else builtins.toString (actualMinorInt + 1)
    ;

  configVersion = let

    major = lib.versions.major version;

  in "${major}.${configMinor}";

  defaultConfigPath = "${haskellModules}/configuration-ghc-${configVersion}.x.nix";

  explainBump =
    lib.optionalString (actualMinor != configMinor)
    " (The minor was incremented since odd numbers are usually git commits that nixpkgs maps to the next release)"
    ;

  cannotInferConfig = ''
  Nixpkgs doesn't contain a compiler configuration file for GHC ${version} (Hix name "${configName}").
  The inferred path is:
  ${defaultConfigPath}${explainBump}
  Please specify the configuration manually in the option '${optionPrefix}build.compilerConfig'.
  '';

  defaultConfig =
    if builtins.pathExists defaultConfigPath
    then defaultConfigPath
    else throw cannotInferConfig
    ;

  # --------------------------------------------------------------------------------------------------------------------
  # Main

  compiler = let

    builder = callPackage (import "${pkgs.path}/pkgs/development/compilers/ghc/common-hadrian.nix" fetchArgs);

    builderDefaultArgs = {
      bootPkgs = pkgsBuildBuild.haskell.packages.${conf.bootCompiler};
      inherit (buildPackages.python3Packages) sphinx;
      inherit (buildPackages.darwin) xattr autoSignDarwinBinariesHook;
      buildTargetLlvmPackages = pkgsBuildTarget.llvmPackages_19;
      llvmPackages = pkgs.llvmPackages_19;
    };

  in warnHash (builder (builderDefaultArgs // sourceArg // flavourArg // conf.builderArgs));

  packages = callPackage haskellModules {
    buildHaskellPackages = buildPackages.haskell.packages.${tag};
    ghc = buildPackages.haskell.compiler.${tag};
    compilerConfig = util.fromMaybeNull (callPackage defaultConfig {}) conf.compilerConfig;
  };

in { inherit compiler packages; }
