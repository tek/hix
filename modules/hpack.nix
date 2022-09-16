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

    hasMain = dir: pathExists "${pkg.src}/${dir}/Main.hs";

    hasNoMain = dir: pathExists "${pkg.src}/${dir}" && !(hasMain dir);

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

    amend = type: dirs: key: dir: conf: name: let
      notFound = throw ''
        The ${type} '${name}' was specified without a 'source-dirs' attribute.
        Hix chooses either ${dirs} in this case, if one of those directories contains 'Main.hs'.
        These conditions are not satisfied.
      '';
    in
    if dir == null
    then notFound
    else over [key name] (a: a // mkExe dir) conf;

    inferExe = dir: optionalAttrs (dir != null) { executables.${pname} = mkExe dir; };

    checkExe = conf: let
      inferredDir = findFirst hasMain null ["app" "src"];
      comps = conf.executables;
      exe = head (attrNames comps);
      new =
        if hasAttr "executables" conf
        then
          if length (attrNames comps) == 1 && !(hasAttr "source-dirs" comps.${exe})
          then amend "executable" "'app' or 'src'" "executables" inferredDir conf exe
          else conf
        else conf // inferExe inferredDir
        ;
    in over ["executables"] (mapAttrs (_: ensureDeps (conf ? library))) new;

    inferTest = dir: optionalAttrs (dir != null) { tests."${pname}-${dir}" = mkExe dir; };

    checkTest = conf: let
      inferredDir = findFirst hasMain null ["test" "tests" "integration"];
      test = head (attrNames conf.tests);
      new =
        if hasAttr "tests" conf
        then
          if length (attrNames conf.tests) == 1 && !(hasAttr "source-dirs" conf.tests.${test})
          then amend "test" "'test', 'tests' or 'integration'" "tests" inferredDir conf test
          else conf
        else conf // inferTest inferredDir
        ;
    in over ["tests"] (mapAttrs (_: ensureDeps (conf ? library))) new;

    amendLib = conf: let
      noLib = throw ''
        The library for '${pname}' was specified without a 'source-dirs' attribute.
        Hix chooses either 'lib' or 'src' in this case, if one of those directories does not contain 'Main.hs'.
        These conditions are not satisfied.
      '';
      inferredDir =
        findFirst hasNoMain noLib ["lib" "src"];
    in over ["library"] (a: a // { source-dirs = inferredDir; }) conf;

    inferLib = let
      use = dir: { library.source-dirs = dir; };
    in
      if hasNoMain "lib"
      then use "lib"
      else if hasNoMain "src"
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

  script = import ../lib/hpack.nix { inherit config; verbose = true; };

  scriptQuiet = import ../lib/hpack.nix { inherit config; };

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

      script = mkOption {
        type = package;
        description = ''
          The script that generates a Cabal file in each of the directories configured in <literal>packages</literal> by
          executing <literal>hpack</literal>.
          It is intended to be run manually in the package directory using <literal>nix run .#hpack</literal>.
          If <literal>hpack.packages</literal> is defined, it is used to synthesize a <literal>package.yaml</literal>.
          Otherwise, the file needs to be present in the source directory.
        '';
      };

      scriptQuiet = mkOption {
        type = package;
        description = ''
          Same as <literal>script</literal>, but suppress all output.
        '';
      };

  };

  config.hpack = {

    packages = mkDefault infer;

    apps =
      mkDefault (outputs: foldl (a: b: a // b) {} (mapAttrsToList (packageApps outputs) config.hpack.packages));

    defaultApp =
      mkDefault config.main;

    script = script;

    scriptQuiet = scriptQuiet;
  };

}
