{ pkgs, ghc, verbose }:
pkgs.writeScript "hpack.zsh" ''
  #!${pkgs.zsh}/bin/zsh
  setopt err_exit no_unset

  base=''${1-''$PWD}
  hpack="$base/ops/hpack"

  gen()
  {
    local dir=$1
    pushd $dir
    cp $hpack/packages/''${dir:t}.yaml $dir/package.yaml
    ln -srf $hpack/shared $dir/shared
    trap "rm -f $dir/package.yaml $dir/shared" EXIT
    ${ghc.hpack}/bin/hpack ${if verbose then "" else "1>/dev/null"}
    popd
  }

  for m ($base/packages/*) gen $m
''
