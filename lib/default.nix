{lib}: let
  self =
  import ./lib/attrs.nix { inherit lib; }
  //
  import ./lib/boot.nix { inherit lib; }
  //
  import ./lib/config.nix { inherit lib; }
  //
  import ./lib/console.nix { inherit self; }
  //
  import ./lib/maybe.nix { inherit self; }
  //
  import ./lib/overrides.nix { inherit lib; }
  //
  import ./lib/string.nix { inherit lib; }
  //
  {
    inherit lib;

    version = import ./lib/version.nix { inherit lib; };
  };

in self
