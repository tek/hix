{util}: let

  inherit (util) internal lib;

  customize = conf: pkgs: default:
  if conf.customize == null
  then default
  else conf.customize (default // { inherit pkgs; })
  ;

  updateWith = f: tag: let
    update = entity: { path = ["haskell" entity tag]; update = _: (f entity); };
  in lib.updateManyAttrsByPath [(update "compiler") (update "packages")];

  updateFrom = output: updateWith (entity: output.${entity});

  createPkgs = conf:
  internal.modules.resolveExtensibleModule "nixpkgs" conf.nixpkgs;

  # --------------------------------------------------------------------------------------------------------------------
  # General outputs

  # This happens before `package-set` so that it can grab the `vanilla` set from `prev`.
  attrsOverlay = tag: prevOverlay: pkgsFinal: pkgsPrev: let
    # Contains the injected compiler, but not the overrides injected downstream.
    # In particular, it contains the vanilla package set.
    intermediate = prevOverlay pkgsFinal pkgsPrev;

    output = {
      compiler = pkgsFinal.buildPackages.haskell.compiler.${tag};
      packages = pkgsFinal.haskell.packages.${tag};
      vanilla = intermediate.haskell.packages.${tag};
    };

  in intermediate // {
    __hix = output;
    # compat
    hixPackages = output.packages;
    hixVanillaPackages = output.vanilla;
  };

  withHixAttrs = conf: final: prev: {
    overlay = attrsOverlay final.tag prev.overlay;
  };

  basic = conf: (createPkgs conf).extend (final: prev: {
    inherit (conf) tag;
    overlays = prev.overlays ++ [final.overlay];
    compiler = final.pkgs.haskell.compiler.${final.tag};
    packages = final.pkgs.haskell.packages.${final.tag};
  });

  # --------------------------------------------------------------------------------------------------------------------
  # Select compiler

  # TODO add test for errors
  noSetError = conf: attr: "The nixpkgs set of the compiler '${conf.name}' does not contain the set 'haskell.${attr}'.";

  noSuchCompilerError = conf: pkgs: attr: let
    allAttrs = lib.attrNames pkgs.haskell.${attr};
    render = lib.concatStringsSep ", ";
    attrs =
      if lib.length allAttrs > 40
      then "${render (lib.take 40 allAttrs)} ..."
      else render allAttrs;
  in ''
  The compiler '${conf.name}' is configured to use the nixpkgs GHC '${conf.source}', but there is no such attribute in 'pkgs.haskell.${attr}'.
  Existing attributes:
  ${attrs}
  '';

  defaultOrError = conf: pkgs: attr: let
    target = pkgs.haskell.${attr} or null;
    value = target.${conf.source} or null;
  in
  if target == null
  then throw (noSetError conf attr)
  else
  if value == null
  then throw (noSuchCompilerError conf pkgs attr)
  else value
  ;

  defaultCompiler = conf: finalPkgs: prevPkgs: {
    compiler = defaultOrError conf prevPkgs "compiler";
    packages = defaultOrError conf prevPkgs "packages";
  };

  customCompiler = conf: tag: finalPkgs: prevPkgs:
  finalPkgs.callPackage (internal.ghc.build {
    configName = conf.name;
    conf = conf.source.build;
    inherit tag;
  }) {};

  selectCompiler = conf: final:
  if conf.source ? manual
  then conf.source.manual
  else if conf.source ? build
  then customCompiler conf final.tag
  else defaultCompiler conf
  ;

  # --------------------------------------------------------------------------------------------------------------------
  # Select overlay source

  withCustomize = conf: final: prev: {
    overlay = pkgsFinal: pkgsPrev: let
      overlayPrev = prev.overlay pkgsFinal pkgsPrev;
      custom = customize conf pkgsFinal {
        compiler = overlayPrev.haskell.compiler.${final.tag};
        packages = overlayPrev.haskell.packages.${final.tag};
      };
    in updateFrom custom final.tag overlayPrev;
  };

  provideOverlay = conf: { inherit (conf) overlay; };

  createOverlay = conf: final: {
    overlay = pkgsFinal: pkgsPrev: let
      output = selectCompiler conf final pkgsFinal pkgsPrev;
    in updateFrom output final.tag pkgsPrev;
  };

  withOverlay = conf: final: prev:
  if conf.source ? overlay
  then provideOverlay conf
  else createOverlay conf final
  ;

  # --------------------------------------------------------------------------------------------------------------------
  # Main

  create = conf:
  (((basic conf).extend (withOverlay conf)).extend (withCustomize conf)).extend (withHixAttrs conf)
  ;

in util.mapValues create util.config.compilers // { __functor = _: create; }
