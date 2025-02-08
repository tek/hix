{config, util, ...}: let
  release = import ../../lib/release.nix { inherit config util; };
in {
  root = false;
  updateLock = false;

  source = ''
  version='0.6.7'
  git init -q
  mkdir ops
  ${release.updateCliVersion}
  file_exact '{
    version = "0.6.7";
    sha256 = "1kcab39rwangb4m1viw58ppvf1ps4i75i2dgapzyqklq87f1bmpi";
  }' ops/cli-dep.nix
  step :
  '';
}
