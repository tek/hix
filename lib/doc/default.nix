{inputs, pkgs, hixUrl}:
let
  lib = pkgs.lib;
  libBase = import ../default.nix { inherit lib; };

  modules =
    [
      { inherit (pkgs) system; }
      (libBase.utilModule {})
    ] ++
    import ../../modules/all-modules.nix { inherit inputs; };

  config = (lib.evalModules { inherit modules; }).config;

  util = import ../util.nix { inherit config lib; };

  prose = import ./prose.nix { inherit hixUrl; };

  chapters = import ./chapters.nix { inherit config pkgs util prose; };

  render = import ./render.nix { inherit pkgs util chapters; inherit (prose) header; };

in {
  json = render.optionsJSON;
  html = render.renderManual;
}
