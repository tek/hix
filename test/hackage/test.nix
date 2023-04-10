{ pkgs }:
let

  regular = ''
    cd ./root
    nix flake update
    git init --quiet
    git add .
    git commit -m "init" --quiet

    output=$(run .#release | head -n2)
    if [[ $output != $pristine_target ]]
    then
      fail "Wrong output for release-all-pristine-version commands:\n$output"
    fi

    output=$(git show --format=format:%s --no-patch)
    if [[ $output != "Release $pristine_version" ]]
    then
      fail "Wrong version commit message for release-all-pristine-version:\n$output"
    fi

    output=$(git tag -l --points-at=HEAD)
    if [[ $output != "v$pristine_version" ]]
    then
      fail "No tag with name "v$pristine_version" points at HEAD for release-all-pristine-version:\n$output"
    fi

    git reset --quiet --hard @^

    output=$(run .#release -- -v $version | head -n2)
    if [[ $output != $version_target ]]
    then
      fail "Wrong output for release-all-version commands:\n$output"
    fi

    output=$(git show --format=format:%s --no-patch)
    if [[ $output != "Release $version" ]]
    then
      fail "Wrong version commit message for release-all-version:\n$output"
    fi

    output=$(git tag -l --points-at=HEAD)
    if [[ $output != "v$version" ]]
    then
      fail "No tag with name "v$version" points at HEAD for release-all-version:\n$output"
    fi

    sed -i "s/^version:\(\s*\).*/version:\1$manual_version/" root.cabal
    git add .
    git commit -m "bump manually" --quiet

    output=$(run .#release | head -n2)
    if [[ $output != $manual_target ]]
    then
      fail "Wrong output for release-all-version-manual commands:\n$output"
    fi

    output=$(git show --format=format:%s --no-patch)
    if [[ $output != "Release $manual_version" ]]
    then
      fail "Wrong version commit message for release-all-version-manual:\n$output"
    fi

    output=$(git tag -l --points-at=HEAD)
    if [[ $output != "v$manual_version" ]]
    then
      fail "No tag with name "v$manual_version" points at HEAD for release-all-version-manual:\n$output"
    fi

    git reset --quiet --hard @^^^

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
    if [[ $output != "Release root $version" ]]
    then
      fail "Wrong version commit message for release-one:\n$output"
    fi

    output=$(git tag -l --points-at=HEAD)
    if [[ $output != "root-v$version" ]]
    then
      fail "No tag with name "root-v$version" points at HEAD for release-one:\n$output"
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
    nix run .#hpack-quiet
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
  test = builtins.toFile "hackage-test" ''
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

    pristine_version='0.1.0.0'
    pristine_target="$(src_target true $pristine_version)
    $(doc_target true $pristine_version)"
    version='1.0.0.0'
    version_target="$(src_target true $version)
    $(doc_target true $version)"
    manual_version="2.0.0.0"
    manual_target="$(src_target true $manual_version)
    $(doc_target true $manual_version)"

    ${regular}

    cd ..

    ${synthetic}
  '';
}
