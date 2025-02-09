{util}: let

  inherit (util) config;

in {
  base =
    if config.base == null
    then util.path.inferBase
    else config.base
    ;

  packages = import ./packages.nix { inherit util; };
}
