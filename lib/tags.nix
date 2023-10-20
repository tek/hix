{config}:
let

  inherit (config.envs.dev.ghc) compiler pkgs ghc;

  thax = import (builtins.fetchTarball {
    url = "https://github.com/tek/thax/archive/10fa7482d171739414fc0d829005b4055799e4c6.tar.gz";
    sha256 = "1s1pyblys4mba8cws2fbzmby2k6paxdxls2gcyrhh8dcgbymqmwb";
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
    pkgs.writeScript "tags" ''
      #!${pkgs.zsh}/bin/zsh
      setopt err_exit
      destination=''${1:-.tags}
      cp ${projectTags}/tags $destination
      chmod u+w $destination
    '';
}
