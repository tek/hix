{config, util}:
{verbose}:
with builtins;
let
  inherit (config.hpack.internal) packages;
  inherit (config.internal) pkgs;

  packageCall = n: p:
  if hasAttr n packages
  then "synthetic ${n} ${p} ${toFile "package.yaml" (toJSON packages.${n})}"
  else "regular ${n} ${p}";

  packageCalls =
    pkgs.lib.mapAttrsToList packageCall config.internal.relativePackages;

in util.zscript "hpack.zsh" ''
  setopt no_unset

  base=''${1-''$PWD}

  info()
  {
    ${if verbose then ''echo ">>> $*" >&2'' else ""}
  }

  error()
  {
    echo ">>> $*" >&2
  }

  run()
  {
    ${config.internal.basicGhc.hpack}/bin/hpack --force ${if verbose then ">&2" else "1>/dev/null"}
  }

  regular()
  {
    local name=$1 rel=$2 dir="$base/$rel"
    pushd $dir
    info "$dir"
    if [[ -f package.yaml ]]
    then
      run
    else
      error "no package.yaml in $dir"
    fi
    popd
  }

  synthetic()
  {
    local name=$1 rel=$2 file=$3
    local dir="$base/$rel"
    pushd $dir
    info "$dir"
    local remove="$dir/package.yaml"
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

  info 'Generating Cabal files...'
  ${concatStringsSep "\n" packageCalls}
''
