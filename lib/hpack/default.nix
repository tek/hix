{util}:
let
  gen = import ./gen.nix { inherit (util) config; inherit util; };
  conf = import ./conf.nix { inherit util; };
in {
  inherit gen conf;
}
