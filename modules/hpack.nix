{ lib, config, foldAttrs, ... }:
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

  inferPackageConf = pname: src: let
    srcContent = readDir src;

    dirs = filterAttrs (_: type: type == "directory") srcContent;

    files = filterAttrs (_: type: type == "regular") srcContent;

    hsInRoot = any (hasSuffix ".hs") (filter (f: f != "Setup.hs") files);

    test = if dirs ? test then "test" else if dirs ? tests then "tests" else null;

    isExe = d: hasAttr d dirs && pathExists "${src}/${d}/Main.hs";

    exe = if isExe "app" then "app" else if isExe "src" then "src" else null;

    library = if dirs ? lib then "lib" else if dirs ? src && exe != "src" then "src" else null;

    inSubdir = test != null || exe != null || library != null;

    inferLib =
      if library == null
      then {}
      else { library = { source-dirs = library; } // baseDep; };

    exeConf = { main = "Main.hs"; dependencies = [base] ++ (if library == null then [] else [pname]); };

    inferExe =
      if exe == null
      then {}
      else { executables.${pname} = { source-dirs = exe; } // exeConf; };

    inferTest =
      if test == null
      then {}
      else { tests."${pname}-test" = { source-dirs = test; } // exeConf; };

    inferSubdirs = inferLib // inferExe // inferTest;

    inferRoot =
      if pathExists "${src}/Main.hs"
      then { executables.${pname} = { source-dirs = "."; main = "Main.hs"; } // baseDep; }
      else { library = { source-dirs = "."; } // baseDep; }
      ;

    projectDesc = if pname == "project" then "" else " of '${pname}'";

    error = throw ''
    Could not detect the project structure${projectDesc}!
    The directory does not contain any of the directories 'app', 'src', 'lib', 'test' or 'tests', or there is
    no 'Main.hs' in the directories 'app', 'test' or 'tests'.
    '';

  in defaultMeta // { name = pname; } // (if inSubdir then inferSubdirs else inferRoot);

  infer =
    mapAttrs inferPackageConf config.packages;

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
