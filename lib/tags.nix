{ config }:
let
  inherit (config.inputs) thax;
  inherit (config.devGhc) compiler pkgs ghc;

  tags = thax.tags { inherit pkgs compiler; };

  withPrefix =
    name: dir:
    let
      p = ghc.${name};
    in
      if dir == "."
      then p
      else p // { tagsPrefix = dir; };

  projectTags =
    tags.combined.all {
      targets = pkgs.lib.attrsets.mapAttrsToList withPrefix config.internal.relativePackages;
    };

in {
  inherit projectTags;

  app =
    pkgs.writeScript "tags" ''
      #!${pkgs.zsh}/bin/zsh
      setopt err_exit
      destination=''${1:-.tags}
      cp ${projectTags}/tags $destination
      chmod u+w $destination
    '';
}
