{pkgs}: {

  releaseFixtures = ''
  doc_target() {
    local v=''${2:-0.1.0.0}
    cat <<EOF
  {"doc":true,"path":"/nix/store/hash-root-''${v}-haddock/root-''${v}-docs.tar.gz","publish":$1}
  EOF
  }
  src_target() {
    local v=''${2:-0.1.0.0}
    cat <<EOF
  {"doc":false,"path":"/nix/store/hash-root-''${v}-sdist/root-''${v}.tar.gz","publish":$1}
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
  '';

}
