{util}: let

  inherit (util) config internal;

in {
  base =
    if config.base == null
    then internal.path.inferBase
    else config.base
    ;

  packages = import ./packages.nix { inherit util; };
}
