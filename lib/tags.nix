{config, util}:
let

  inherit (util) project build;
  inherit (build.envs.dev.toolchain) tag pkgs packages;

  thax = import (builtins.fetchTarball {
    url = "https://github.com/tek/thax/archive/806e9d5a03a168717e0488fad924942774ace90f.tar.gz";
    sha256 = "1xqk5q477asldpc70br8clk2h3750kby8rc0ddakz27xbr9lkif6";
  }) { inherit pkgs; compiler = tag; hasktagsPackage = config.build-tools.hasktags.package; };

  withPrefix =
    name: pkg:
    let
      p = packages.${name};
    in
      if pkg.path == "."
      then p
      else p // { tagsPrefix = pkg.path; };

  projectTags =
    thax.combined.all {
      targets = pkgs.lib.attrsets.mapAttrsToList withPrefix project.packages;
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
