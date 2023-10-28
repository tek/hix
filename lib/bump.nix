{util}: let
  inherit (util) config lib;

  cli = config.internal.hixCli.exe;

  file = config.managedDeps.file;

  update = lib.optionalString config.managedDeps.update "--update-project";

  verbose = lib.optionalString config.managedDeps.verbose "--verbose";

  debug = lib.optionalString config.managedDeps.debug "--debug";

  overrides = env: lib.optionalString env.managedOverrides "--overrides";

  withCheck = name: main: let
    script =
      if config.managedDeps.enable
      then main
      else ''
      ${util.loadConsole}
      die "Set $(blue 'managedDeps.enable = true;') $(red 'to use this feature.')"
      '';
  in config.pkgs.writeScript "bump-${name}" script;

  mainScript = env: pkg: let
    general = [
      "--config ${util.json.bumpFile}"
      "--env ${env.name}"
      "--file ${file}"
      update
      (overrides env)
    ];
    args = util.unwords (general ++ pkg);
  in
  ''
  set -e
  ${cli} ${verbose} ${debug} bump ${args} $@
  ${lib.optionalString (config.managedDeps.generate && config.managedDeps.update) "nix run .#gen"}
  '';

  packageScript = env: pkg: mainScript env ["--package ${pkg.name}"];

  package = env: pkg:
  withCheck "${env.name}-${pkg.name}" (packageScript env pkg);

  all = env: withCheck "${env.name}-all" (mainScript env []);

in {
  inherit package all;
}
