{ pkgs, cli, extensions ? null, extraCode ? "" }:
with builtins;
with pkgs.lib;
let
  pragma = name: text:
    "{-# ${name} ${text} #-}";

  explicitExtensions = extensions != null;

  extensionPragma =
    if !explicitExtensions
    then "$(${cli} preproc $orig_file)"
    else
    if length extensions != 0
    then pragma "language" (concatStringsSep "," extensions)
    else "";

  append = text:
  ''
    cat >> $out_file <<EOF
    ${text}
    EOF
  '';

  file = pkgs.writeScript "ghci-preprocessor" ''
    #!${pkgs.bash}/bin/bash

    orig_file=$1 in_file=$2 out_file=$3 options_ghc=$4
    touch $out_file

    ${append "${extensionPragma}"}

    if [[ -n "$options_ghc" ]]
    then
      ${append (pragma "options_ghc" "$options_ghc")}
    fi
    ${if stringLength extraCode == 0 then "" else append extraCode}
    ${append (pragma "line" ''1 "$orig_file"'')}
    cat $in_file >> $out_file
  '';
in file
