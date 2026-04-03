{lib}:
let

  internalScope = "__hix-internal__";

  flake-utils = import (builtins.fetchTarball {
    url = "https://github.com/numtide/flake-utils/archive/refs/tags/v1.0.0.tar.gz";
    sha256 = "0hynd4rbkbplxzl2a8wb3r8z0h17z2alhhdsam78g3vgzpzg0d43";
  });

  utilWithConfig = {config, extra ? {}}:
  import ../util.nix { inherit config lib extra; };

  utilModule = extra: {config, ...}: let
    util = utilWithConfig { inherit config extra; };
  in {
    _module.args = {
      inherit util;
      inherit (util) internal project build outputs pkgs;
    };
  };

  evalModules = modules: lib.evalModules { inherit modules; };

  evalConfig = modules: (evalModules modules).config;

  deprecatedOption = args0@{path, ...}: moduleArgs@{util, options, config, ...}: let

    inherit (util) internal;
    args = { key = lib.concatStringsSep "." path; } // lib.removeAttrs args0 ["path"];
    optionArgs = { type = lib.types.str; replacement = null; } // args;

  in {
    options = lib.setAttrByPath path (internal.modules.deprecatedOption optionArgs);
    config.ui.assertions = internal.modules.deprecatedOptionAssertion path args moduleArgs;
  };

in {
  inherit
  internalScope
  flake-utils
  utilWithConfig
  utilModule
  evalModules
  evalConfig
  deprecatedOption
  ;
}
