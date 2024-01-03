{util}:
let
  gen = import ./gen.nix { inherit (util) config; };
  conf = import ./conf.nix { inherit util; };
in {
  inherit gen conf;
}
