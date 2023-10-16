{config, util, ...}: let
  inherit (config) pkgs;
  release = import ../../lib/release.nix { inherit config util; };
in {
  test = pkgs.writeText "update-cli-version-test" ''
    version='0.6.7'
    git init -q
    mkdir ops
    ${release.updateCliVersion}
    target='{
      version = "0.6.7";
      sha256 = "1kcab39rwangb4m1viw58ppvf1ps4i75i2dgapzyqklq87f1bmpi";
    }'
    check 'cat ops/cli-dep.nix' "$target" 'Content of cli-dep.nix'
  '';
}
