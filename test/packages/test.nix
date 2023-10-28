{...}: {
  test = builtins.toFile "packages-test" ''
    cd ./root
    flake_update
    nix run .#gen-cabal-quiet

    check_diff ${./dep.cabal} dep/dep-lib.cabal "The generated Cabal file for 'dep-lib' differs from the target."

    check_diff ${./root.cabal} root.cabal "The generated Cabal file for 'root' differs from the target."

    check 'nix run .#run' 'string/lib2/lib1' 'Output is wrong'
  '';
}
