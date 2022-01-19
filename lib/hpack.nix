{ pkgs, ghc, verbose, paths, dir, shared ? "shared", }:
let
  packageCalls = pkgs.lib.mapAttrsToList (n: p: "gen ${n} ${p}") paths;
in pkgs.writeScript "hpack.zsh" ''
  #!${pkgs.zsh}/bin/zsh
  setopt err_exit no_unset

  base=''${1-''$PWD}
  hpack="$base/${dir}"
  shared="$hpack/${shared}"

  run()
  {
    ${ghc.hpack}/bin/hpack ${if verbose then "" else "1>/dev/null"}
  }

  gen_from_dir()
  {
    local name=$1 dir=$2
    remove="$dir/package.yaml"
    cp $hpack/packages/$name.yaml $dir/package.yaml
    if [[ -d $shared ]]
    then
      ln -srf $shared $dir/${shared}
      remove="$remove $dir/${shared}"
    fi
    trap "rm -f $remove" ZERR
    trap "rm -f $remove" EXIT
    run
  }

  gen()
  {
    local name=$1 rel=$2
    dir="$base/$rel"
    pushd $dir
    if [[ -f package.yaml ]]
    then
      run
    elif [[ -d $hpack ]]
    then
      gen_from_dir $name $dir
    else
      echo "no package.yaml in $dir and no hpack dir present at $hpack"
    fi
    popd
  }

  ${builtins.concatStringsSep "\n" packageCalls}
''
