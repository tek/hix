{config, util, ...}: let
  release = import ../../lib/release.nix { inherit config util; };
in {
  root = false;
  updateLock = false;

  source = ''
  export version='0.6.7'
  export context='{"publish":{"sources":true}}'
  git init -q
  mkdir ops
  file_exact '{
    version = "0.6.7";
    sha256 = "1kcab39rwangb4m1viw58ppvf1ps4i75i2dgapzyqklq87f1bmpi";
  }' ops/cli-dep.nix
  step ${release.hookUpdateCliVersion}
  '';
}
