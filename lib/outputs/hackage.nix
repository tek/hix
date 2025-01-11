{util}: let

  logic = import ../hackage.nix { inherit util; };

in {

  legacyPackages = logic.legacyPackages;

  apps = logic.apps;

}
