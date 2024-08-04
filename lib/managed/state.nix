{util}: let
  inherit (util) config lib;

  current = let
    file = "${util.project.base}/${config.managed.file}";
  in
  { bounds = {}; overrides = {}; initial = {}; resolving = false; } //
  lib.optionalAttrs (config.managed.enable && lib.pathExists file) (import file);

in {
  inherit current;
}
