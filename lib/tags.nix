{config, util}:
let

  inherit (config.envs.dev.ghc) compiler pkgs ghc;

  thax = import (builtins.fetchTarball {
    url = "https://github.com/tek/thax/archive/806e9d5a03a168717e0488fad924942774ace90f.tar.gz";
    sha256 = "1xqk5q477asldpc70br8clk2h3750kby8rc0ddakz27xbr9lkif6";
  }) { inherit pkgs compiler; };

  withPrefix =
    name: dir:
    let
      p = ghc.${name};
    in
      if dir == "."
      then p
      else p // { tagsPrefix = dir; };

  projectTags =
    thax.combined.all {
      targets = pkgs.lib.attrsets.mapAttrsToList withPrefix config.internal.relativePackages;
    };

in {
  inherit projectTags;

  app =
    util.zscript "tags" ''
      destination=''${1:-.tags}
      cp ${projectTags}/tags $destination
      chmod u+w $destination
    '';
}
