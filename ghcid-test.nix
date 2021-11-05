{ pkgs, hpack ? true, pre ? "", }:
{
  testApp =
    pkgs.writeScript "ghcid-app" ''
      #!${pkgs.zsh}/bin/zsh
      pkg=$1 module=$2 name=$3 type_=$4 runner=''${5-generic}
      ${pre}
      ${if hpack then "nix run '.#hpack'" else ""}
      nix develop --impure --expr "
        (builtins.getFlake path:$PWD).legacyPackages.x86_64-linux.run.override {
          pkg = \"$pkg\";
          module = \"$module\";
          name = \"$name\";
          type = \"$type_\";
          runner = \"$runner\";
        }" -c exit
  '';
}
