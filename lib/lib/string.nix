{lib}: let

  unwords = lib.concatStringsSep " ";

  unlines = lib.concatStringsSep "\n";

  lines = lib.splitString "\n";

  unlinesMap = lib.concatMapStringsSep "\n";

  unlinesConcatMap = f: xs: lib.concatStringsSep "\n" (lib.concatMap f xs);

  toTitle = s:
  if lib.stringLength s == 0
  then s
  else lib.toUpper (lib.substring 0 1 s) + lib.substring 1 (-1) s;

in {

  inherit
  unwords
  unlines
  lines
  unlinesMap
  unlinesConcatMap
  toTitle
  ;

}
