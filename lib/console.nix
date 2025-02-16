{...}: let

  inherit (builtins) mapAttrs;

  indentLine = l: "  " + l;

  colorOffsets = {
    black = 0;
    red = 1;
    green = 2;
    yellow = 3;
    blue = 4;
    magenta = 5;
    cyan = 6;
    white = 7;
  };

  colorsBased = base: mapAttrs (_: o: base + o) colorOffsets;

  colors = colorsBased 30;

  colorsBg = colorsBased 40;

  colorsBright = colorsBased 90;

  colorsBgBright = colorsBased 100;

  startSgr = n: "\\e[" + toString n + "m";

  resetSgr = startSgr 0;

  sgr = n: t: startSgr n + t + resetSgr;

  bold = sgr "1";

  faint = sgr "2";

  underline = sgr "4";

  colorExt = n: sgr "38;5;${toString n}";

  colorExtBg = n: sgr "48;5;${toString n}";

  # TODO these could be module options!
  aliases = {
    path = "blue";
    shell_cmd = "blue";
    regex = "yellow";
    env = "yellow";
    function = "yellow";
    command = "blue";
    variable = "cyan";
    error = "red";
    option = "magenta";
    package = "blue";
  };

  withAliases = render: let
    palette = mapAttrs (_: render) colors;
  in palette // mapAttrs (_: color: palette.${color}) aliases;

in {

  inherit
  indentLine
  startSgr
  resetSgr
  sgr
  colors
  colorsBg
  colorsBright
  colorsBgBright
  aliases
  bold
  faint
  underline
  colorExt
  colorExtBg
  ;

  color = sgr;

  indent = map indentLine;

  s.colors = withAliases sgr;
  s.colorsBg = mapAttrs (_: sgr) colorsBg;

  start.colors = builtins.mapAttrs (_: startSgr) colors;
  start.colorsBg = builtins.mapAttrs (_: startSgr) colorsBg;

  rgb = r: g: b: sgr "38;2;${r};${g};${b}";
  rgbBg = r: g: b: sgr "48;2;${r};${g};${b}";

  chevrons = bold (sgr colors.magenta ">>>");
  chevronsH = bold (colorExt 55 ">>=");

  chevronY = bold (sgr colors.yellow ">");
  chevronM = bold (sgr colors.magenta ">");

}
