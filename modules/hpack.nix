{ lib, config, foldAttrs, foldMapAttrs, over, ... }:
with builtins;
with lib;
with types;
let

  maybeDefaultApp = name: a:
  if name == config.hpack.defaultApp
  then { default = a; }
  else {};

  app = pkg: name: exe:
  let a = { type = "app"; program = "${pkg}/bin/${name}"; };
  in { ${name} = a; } // maybeDefaultApp name a;

  packageApps = outputs: pname: conf:
  foldAttrs (mapAttrsToList (app (outputs.${pname})) (conf.executables or {}));

  defaultMeta = {
    version = "0.1.0.0";
    license = "GPL-3";
  };

  base = { name = "base"; version = ">= 4 && < 5"; };

  baseDep = { dependencies = [base]; };

  inferPackageConf = pname: pkg: let

    packageDeps = pkg.dependencies or [];
    extraDeps = config.dependencies ++ packageDeps;

    ensureDeps = addLib: comp: let
      deps = extraDeps ++ comp.dependencies or [];
    in comp // { dependencies = optional addLib pname ++ (if deps == [] then [base] else deps); };

    allDirs = ["lib" "src" "app" "test" "tests" "integration"];

    inferDirStatus = dir:
    let srcDir = "${pkg.src}/${dir}";
    in optionalAttrs (pathExists srcDir) { ${dir} = pathExists "${srcDir}/Main.hs"; };

    existing = foldMapAttrs inferDirStatus allDirs;

    specified = let
      check = comp:
        optional (hasAttr "source-dirs" comp) comp.source-dirs;
    in
    (if hasAttr "library" pkg then check pkg.library else []) ++
    concatMap check ((attrValues (pkg.executables or {})) ++ (attrValues (pkg.tests or {})))
    ;

    available = filterAttrs (n: _: !(elem n specified)) existing;

    mkExe = source-dirs: { inherit source-dirs; main = "Main.hs"; };

    amendExe = conf: name: let
      inferredDir =
        if available.app or false
        then "app"
        else if available.src or false
        then "src"
        else throw ''
        The executable ${name} was specified without a 'source-dirs' attribute.
        Hix chooses either 'app' or 'src' in this case, if one of those directories contains 'Main.hs'.
        These conditions are not satisfied.
        '';
    in over ["executables" name] (a: a // mkExe inferredDir) conf;

    inferExe = let
      use = dir: { executables.${pname} = mkExe dir; };
    in
      if available.app or false
      then use "app"
      else if available.src or false
      then use "src"
      else {};

    checkExe = conf: let
      exe = head (attrNames conf.executables);
      new =
        if hasAttr "executables" conf
        then
          if length (attrNames conf.executables) == 1 && !(hasAttr "source-dirs" conf.executables.${exe})
          then amendExe conf exe
          else conf
        else conf // inferExe
        ;
    in over ["executables"] (mapAttrs (_: ensureDeps (conf ? library))) new;

    amendTest = conf: name: let
      inferredDir =
        if available.test or false
        then "test"
        else if available.tests or false
        then "tests"
        else if available.integration or false
        then "integration"
        else throw ''
        The test ${name} was specified without a 'source-dirs' attribute.
        Hix chooses either 'test', 'tests' or 'integration' in this case, if one of those directories contains 'Main.hs'.
        These conditions are not satisfied.
        '';
    in over ["tests" name] (a: a // mkExe inferredDir) conf;

    inferTest = let
      use = dir: { tests."${pname}-${dir}" = mkExe dir; };
    in
      if available.test or false
      then use "test"
      else if available.tests or false
      then use "tests"
      else if available.integration or false
      then use "integration"
      else {};

    checkTest = conf: let
      test = head (attrNames conf.tests);
      new =
        if hasAttr "tests" conf
        then
          if length (attrNames conf.tests) == 1 && !(hasAttr "source-dirs" conf.tests.${test})
          then amendTest conf test
          else conf
        else conf // inferTest
        ;
    in over ["tests"] (mapAttrs (_: ensureDeps (conf ? library))) new;

    amendLib = conf: let
      inferredDir =
        if !(available.lib or true)
        then "lib"
        else if !(available.src or true)
        then "src"
        else throw ''
        The library for ${pname} was specified without a 'source-dirs' attribute.
        Hix chooses either 'lib' or 'src' in this case, if one of those directories does not contain 'Main.hs'.
        These conditions are not satisfied.
        '';
    in over ["library"] (a: a // { source-dirs = inferredDir; }) conf;

    inferLib = let
      use = dir: { library.source-dirs = dir; };
    in
      if !(available.lib or true)
      then use "lib"
      else if !(available.src or true)
      then use "src"
      else {};

    checkLib = conf: let
      new =
        if hasAttr "library" conf
        then
          if !(hasAttr "source-dirs" conf.library)
          then amendLib conf
          else conf
        else conf // inferLib
        ;
    in over ["library"] (ensureDeps false) new;

    subdirs = (checkTest (checkExe (checkLib pkg)));

    root =
      if pathExists "${pkg.src}/Main.hs"
      then { executables.${pname} = ensureDeps false (mkExe "."); }
      else { library = ensureDeps false { source-dirs = "."; }; }
      ;

    couldInfer = subdirs ? library || subdirs ? executables || subdirs ? tests;

    full = defaultMeta // { name = pname; } // (if couldInfer then subdirs else root);

    clean = removeAttrs full ["dependencies" "src"];

  in clean;

  infer =
    mapAttrs inferPackageConf config.internal.packages;

in {

  options.hpack = {

      packages = mkOption {
        type = attrsOf unspecified;
      };

      apps = mkOption {
        type = functionTo unspecified;
        default = _: {};
      };

      defaultApp = mkOption {
        type = str;
        description = ''
          The name of an executable in <literal>hpack.packages</literal> that should be assigned to
          <literal>packages.default</literal>.
        '';
      };

  };

  config.hpack = {

    packages = mkDefault infer;

    apps =
      mkDefault (outputs: foldl (a: b: a // b) {} (mapAttrsToList (packageApps outputs) config.hpack.packages));

    defaultApp =
      mkDefault config.main;

  };

}
