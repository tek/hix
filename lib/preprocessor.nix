{
  pkgs,
  cli,
  options_ghc ? "",
  extraCode ? "",
}:

with builtins;
with pkgs.lib;

let

  option = name: value:
  if value == ""
  then ""
  else "--${name} '${value}'";

  optionsOption = option "options" options_ghc;

  extraOption = option "extra" extraCode;

  file = pkgs.writeScript "ghci-preprocessor" ''
    #!${pkgs.bash}/bin/bash
    ${cli} preproc --source "$1" --in "$2" --out "$3" ${optionsOption} ${extraOption}
  '';

in file
