{util}:
let
  inherit (util) lib project config;
  inherit (config.hpack.internal) packages;

  packageCall = n: p:
  if lib.hasAttr n packages
  then "synthetic ${n} ${p} ${builtins.toFile "package.yaml" (builtins.toJSON packages.${n})}"
  else "regular ${n} ${p}";

  packageCalls =
    lib.mapAttrsToList packageCall (util.mapValues (p: p.path) project.packages);

  gen = {verbose}:
  util.zscript "hpack.zsh" ''
  setopt no_unset
  ${util.setupScript { inherit verbose; }}

  base=''${1-''$PWD}

  run()
  {
    ${config.build-tools.hpack.package}/bin/hpack --canonical --force ${if verbose then ">&2" else "1>/dev/null"}
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
  ${lib.concatStringsSep "\n" packageCalls}
  '';

in {
  inherit gen;
}
