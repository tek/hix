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
  ${util.setupScript { inherit verbose; }}

  base=''${1-''$PWD}

  run()
  {
    ${config.internal.basicGhc.hpack}/bin/hpack --canonical --force ${if verbose then ">&2" else "1>/dev/null"}
  }

  regular()
  {
    local name=$1 rel=$2 dir="$base/$rel"
    pushd $dir
    message "$dir"
    if [[ -f package.yaml ]]
    then
      run
    else
      error_message "No $(color_path package.yaml) in $(color_path $dir)"
    fi
    popd
  }

  synthetic()
  {
    local name=$1 rel=$2 file=$3
    local dir="$base/$rel"
    pushd $dir
    message "$dir"
    local remove="$dir/package.yaml"
    cp $file package.yaml
    fail() {
      cat $file
      rm -f $remove
    }
    trap fail ZERR
    trap "rm -f $remove" EXIT
    run
    popd
  }

  message 'Generating Cabal files...'
  ${concatStringsSep "\n" packageCalls}
''
