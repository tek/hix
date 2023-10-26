{pkgs, ...}: let

  direnv = "${pkgs.direnv}/bin/direnv";

in {
  test = pkgs.writeText "direnv-test" ''
    cd ./root
    flake_update
    cat > .envrc <<EOF
    source ${pkgs.nix-direnv}/share/nix-direnv/direnvrc
    use flake
    EOF
    eval "$(${direnv} hook zsh)"
    ${direnv} allow .
    cd . &>/dev/null
    check_match 'which ghc' 'bin/ghc' 'GHC not present in direnv'
  '';
}
