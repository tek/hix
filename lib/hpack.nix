{ config }:
with builtins;
let
  inherit (config.hpack) packages;
  inherit (config) pkgs;

  packageCall = n: p:
  if hasAttr n packages
  then "synthetic ${n} ${p} ${toFile "package.yaml" (toJSON packages.${n})}"
  else "regular ${n} ${p}";

  packageCalls =
    pkgs.lib.mapAttrsToList packageCall config.internal.relativePackages;

in pkgs.writeScript "hpack.zsh" ''
  #!${pkgs.zsh}/bin/zsh
  setopt err_exit no_unset

  base=''${1-''$PWD}

  run()
  {
    ${config.internal.basicGhc.hpack}/bin/hpack
  }

  regular()
  {
    local name=$1 rel=$2
    dir="$base/$rel"
    pushd $dir
    echo ">>> $dir"
    if [[ -f package.yaml ]]
    then
      run
    else
      echo "no package.yaml in $dir"
    fi
    popd
  }

  synthetic()
  {
    local name=$1 rel=$2 file=$3
    dir="$base/$rel"
    pushd $dir
    echo ">>> $dir"
    remove="$dir/package.yaml"
    cp $file package.yaml
    error() {
      cat $file
      rm -f $remove
    }
    trap error ZERR
    trap "rm -f $remove" EXIT
    run
    popd
  }

  ${concatStringsSep "\n" packageCalls}
''
