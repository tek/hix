{pkgs, ...}: {
  path = pkgs: [pkgs.direnv];

  source = ''
    cat > .envrc <<EOF
    source ${pkgs.nix-direnv}/share/nix-direnv/direnvrc
    use flake
    EOF

    eval "$(direnv hook zsh)"

    step direnv allow .

    cd . &>/dev/null

    describe "GHC is in path in direnv"
    output_match 'bin/ghc'
    step which ghc

    cd .. &>/dev/null
  '';
}
