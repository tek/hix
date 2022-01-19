{ pkgs, extensions ? [], extraCode ? "" }:
with builtins;
with pkgs.lib;
let
  pragma = name: text:
    "{-# ${name} ${text} #-}";

  extensionPragma =
    pragma "language" (concatStringsSep "," extensions);

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

    ${if length extensions == 0 then "" else append extensionPragma}
    ${append (pragma "options_ghc" "$options_ghc")}
    ${if stringLength extraCode == 0then "" else append extensionPragma}
    ${append (pragma "line" ''1 "$orig_file"'')}
    cat $in_file >> $out_file
  '';
in file
