{
  git = true;

  source = ''
    cd sub

    step_run gen-cabal-quiet
  '';
}
