{ lib, config, util, ... }:
let
  inherit (lib) types mkOption;

  libOutput = util.output;

  maybeDefaultApp = name: a:
  if name == config.defaultApp
  then { default = a; }
  else {};

  # TODO this is too out of place here
  appWithAppimage = pkg: name:
  util.app "${pkg}/bin/${name}" // libOutput.appimageApp "dev" name;

  appField = pkg: name: exe:
  let a = appWithAppimage pkg name;
  in { ${name} = a; } // maybeDefaultApp name a;

  packageApps = outputs: pname: conf: let
    main = libOutput.pkgMainExe config.packages.${pname};
    pkg = outputs.${pname};
  in
  lib.optionalAttrs (main != null) { ${pname} = appWithAppimage pkg main.name; } //
  util.catAttrs (lib.mapAttrsToList (appField pkg) (conf.executables or {}))
  ;

in {
  options = {

    defaultApp = mkOption {
      type = types.str;
      description = ''
      The name of an executable in [](#opt-general-packages) that should be assigned to `packages.default`.
      '';
    };

    hpack = {

      apps = mkOption {
        type = types.lazyAttrsOf util.types.flakeApp;
        default = {};
      };

      script = mkOption {
        type = types.path;
        description = ''
          The script that generates a Cabal file in each of the directories configured in `packages` by
          executing `hpack`.
          It is intended to be run manually in the package directory using `nix run .#hpack`.
          If `hpack.packages` is defined, it is used to synthesize a `package.yaml`.
          Otherwise, the file needs to be present in the source directory.
        '';
      };

      scriptQuiet = mkOption {
        type = types.path;
        description = ''
          Same as `script`, but suppress all output.
        '';
      };

      internal.packages = mkOption {
        type = types.attrsOf util.types.strict;
      };
    };
  };

  config = {

    defaultApp = lib.mkDefault config.main;

    hpack = {

      apps = lib.mkDefault (
        let drvs = util.env.derivations "apps" "dev";
        in util.catAttrs (lib.mapAttrsToList (packageApps drvs) config.hpack.internal.packages)
      );

      script = util.hpack.gen { verbose = true; };

      scriptQuiet = util.hpack.gen { verbose = false; };

      internal.packages =
        if config.managed.enable
        then util.hpack.conf.packagesWithManaged
        else util.hpack.conf.packages
        ;

    };
  };
}
