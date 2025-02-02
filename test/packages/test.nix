{...}: {
  source = ''
    cd ./root
    flake_update

    describe "Generate Cabal files for $(blue dep-lib) and $(blue root)"
    file_exact ${./dep.cabal} dep/dep-lib.cabal
    file_exact ${./root.cabal} root.cabal
    step_run gen-cabal-quiet

    describe "Run executable $(blue run) whose dependencies are added via $(yellow cabal.meta)"
    output_exact 'string/lib2/lib1'
    step_run run
  '';
}
