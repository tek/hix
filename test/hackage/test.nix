{ pkgs }:
let

  regular = ''
    cd ./root
    nix flake update
    git init --quiet
    git add .
    git commit -m "init" --quiet

    output=$(run .#release -- -v $version | head -n2)
    if [[ $output != $version_target ]]
    then
      fail "Wrong output for release-all-version commands:\n$output"
    fi

    output=$(git show --format=format:%s --no-patch)
    if [[ $output != "v$version" ]]
    then
      fail "Wrong version commit message for release-all:\n$output"
    fi

    output=$(git tag -l --points-at=HEAD)
    if [[ $output != "v$version" ]]
    then
      fail "No tag with message "v$version" points at HEAD for release-all:\n$output"
    fi

    git reset --quiet --hard @^

    output=$(run .#candidates)
    if [[ $output != $candidates_target ]]
    then
      fail "Wrong output for candidates-all commands:\n$output"
    fi

    output=$(run .#release -- root -v $version | head -n2)
    if [[ $output != $version_target ]]
    then
      fail "Wrong output for release-one commands:\n$output"
    fi

    output=$(git show --format=format:%s --no-patch)
    if [[ $output != "root v$version" ]]
    then
      fail "Wrong version commit message for release-one:\n$output"
    fi

    git reset --quiet --hard @^

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

  synthetic = ''
    cd ./nix-hpack
    nix flake update
    nix run .#hpack
    git init --quiet
    git add .
    git commit -m "init" --quiet

    output=$(run .#release -- -v $version | head -n2)
    if [[ $output != $version_target ]]
    then
      fail "Wrong output for release-all-version-nix-hpack commands:\n$output"
    fi
    if [[ "\"$version\"" != $(cat version.nix) ]]
    then
      fail "Version file wasn't updated for release-all-version-nix-hpack."
    fi
  '';

in {
  test = ''
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

    ${regular}

    cd ..

    ${synthetic}
  '';
}
