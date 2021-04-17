inputs:
{
  packages,
  pkgs,
  ghc,
  compiler,
}:
let
  tags = inputs.thax.tags { inherit pkgs compiler; };

  withPrefix =
    name: dir:
    let
      p = ghc.${name};
    in
      if dir == "."
      then p
      else p // { tagsPrefix = dir; };

  targets =
    pkgs.lib.attrsets.mapAttrsToList withPrefix packages;

  projectTags =
    tags.combined.all { inherit targets; };

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
