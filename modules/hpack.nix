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

in {

  options.hpack = {

      dir = mkOption {
        type = str;
        default = "ops/hpack";
      };

      shared = mkOption {
        type = str;
        default = "shared";
      };

      packages = mkOption {
        type = attrsOf unspecified;
        default = {};
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
        default = config.main;
      };

  };

  config.hpack.apps =
    mkDefault (outputs: foldl (a: b: a // b) {} (mapAttrsToList (packageApps outputs) config.hpack.packages));

}
