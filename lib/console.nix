{...}: let

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

  colorsBased = base: builtins.mapAttrs (_: o: base + o) colorOffsets;

  colors = colorsBased 30;

  colorsBg = colorsBased 40;

  colorsBright = colorsBased 90;

  colorsBgBright = colorsBased 100;

  color = n: t: "\\e[" + toString n + "m" + t + "\\e[0m";

  bold = color "1";

  faint = color "2";

  underline = color "4";

  colorExt = n: color "38;5;${toString n}";

  colorExtBg = n: color "48;5;${toString n}";

in {
  inherit indentLine color colors colorsBg colorsBright colorsBgBright bold faint underline colorExt colorExtBg;

  indent = map indentLine;

  s.colors = builtins.mapAttrs (_: color) colors;
  s.colorsBg = builtins.mapAttrs (_: color) colorsBg;

  rgb = r: g: b: color "38;2;${r};${g};${b}";
  rgbBg = r: g: b: color "48;2;${r};${g};${b}";

  chevrons = bold (color colors.magenta ">>>");
  chevronsH = bold (colorExt 55 ">>=");

  chevronY = bold (color colors.yellow ">");
  chevronM = bold (color colors.magenta ">");
}
