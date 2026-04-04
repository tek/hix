{util}: let

  inherit (util) config internal;

  specifiedBase =
    if config.base != null
    then config.base
    else config.self
    ;

in {
  inherit specifiedBase;

  base =
    if specifiedBase != null
    then specifiedBase
    else internal.path.inferBase
    ;

  packages = import ./packages.nix { inherit util; };

  managedBounds = config.managed.enable && (config.managed.latest.enable || config.managed.lower.enable);
}
