{...}: {
  test = builtins.toFile "dep-versions-test" ''
    cd ./root
    flake_update

    target='
[34mdep[0m
  
  [33mlibrary[0m
    [35maeson[0m [36m>= 2[0m [32m->[0m [31m2.1.2.1[0m
    [35mextra[0m [36m^>= 1.7[0m [32m->[0m [31m1.7.16[0m
    [35muuid[0m [36m== 1.3[0m [32m->[0m [31m1.3.15[0m
    [35mvector[0m [36m>= 0.11 && < 0.13[0m [32m->[0m [31m0.13.1.0[0m

[34mroot[0m
  
  [33mlibrary[0m
    [35mincipit-core[0m [36m>= 0.4 && < 0.6[0m [32m->[0m [31m0.5.1.0[0m
    [35maeson[0m [36m^>= 2.1[0m [32m->[0m [31m2.1.2.1[0m
    [35marray[0m [core library or unknown]'

    check 'nix run .#dep-versions' $target 'Output is wrong'
  '';
}
