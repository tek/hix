{...}: {

  source = ''
    cd ./root
    flake_update

    file_exact ${./a.cabal} a/a.cabal
    step_run gen-cabal-quiet

    rm a/a.cabal
    sed -i 's/managed\.enable = false/managed.enable = true/' flake.nix

    file_exact ${./a.cabal} a/a.cabal
    step_run gen-cabal-quiet
  '';

}
