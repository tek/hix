{ pkgs }:
{
  test = ''
    cd ./root
    nix flake update

    doc_target() {
      local v=''${2:-0.1.0.0}
      cat <<EOF
    {"doc":true,"path":"root-''${v}-haddock/root-''${v}-docs.tar.gz","publish":$1}
    EOF
    }
    src_target() {
      local v=''${2:-0.1.0.0}
      cat <<EOF
    {"doc":false,"path":"root-''${v}-sdist/root-''${v}.tar.gz","publish":$1}
    EOF
    }
    release_target="$(src_target true)
    $(doc_target true)"

    candidates_target="$(src_target false)
    $(doc_target false)"

    run() {
      nix run $* | sed 's#/nix/store/[^-]\+-##g'
    }

    version='1.0.0.0'
    version_target="$(src_target true $version)
    $(doc_target true $version)"

    output=$(run .#release -- -v $version)
    if [[ $output != $version_target ]]
    then
      fail "Wrong output for release-all-version commands:\n$output"
    fi

    sed -i 's#^version:.*#version: 0.1.0.0#' root.cabal

    output=$(run .#candidates)
    if [[ $output != $candidates_target ]]
    then
      fail "Wrong output for candidates-all commands:\n$output"
    fi

    output=$(run .#release -- root -v $version)
    if [[ $output != $version_target ]]
    then
      fail "Wrong output for release-one commands:\n$output"
    fi

    sed -i 's#^version:.*#version: 0.1.0.0#' root.cabal

    output=$(run .#docs)
    if [[ $output != $(doc_target true) ]]
    then
      fail "Wrong output for docs commands:\n$output"
    fi

    output=$(run .#docs -- root)
    if [[ $output != $(doc_target true) ]]
    then
      fail "Wrong output for docs commands:\n$output"
    fi
  '';
}
