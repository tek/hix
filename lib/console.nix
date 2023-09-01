{...}: let

  indentLine = l: "  " + l;

  colors = {
    green = "32";
    yellow = "33";
    blue = "34";
    magenta = "35";
  };

  color = n: t: "\\e[" + toString n + "m" + t + "\\e[0m";

in {
  inherit indentLine color colors;

  indent = map indentLine;

  bold = color "1";

}
