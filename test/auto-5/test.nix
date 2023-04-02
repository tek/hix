{ pkgs }:
{
  test = builtins.toFile "auto-5-test" ''
    cd ./root
    nix flake update

    target='[ "ghc8107" "ghc902" "ghc925" "ghc943" "root" ]'
    check 'nix eval .#checks.x86_64-linux --apply builtins.attrNames' $target 'Checks are wrong'

    target='[ "root" ]'
    check 'nix eval .#checks.x86_64-linux.ghc925 --apply builtins.attrNames' $target 'Checks for ghc925 are wrong'

    target='[ "default" "ghc8107" "ghc902" "ghc925" "ghc943" "min" "root" ]'
    check 'nix eval .#packages.x86_64-linux --apply builtins.attrNames' $target 'Packages are wrong'

    target='[ "root" ]'
    check 'nix eval .#packages.x86_64-linux.ghc925 --apply builtins.attrNames' $target 'Packages for ghc925 are wrong'

    nix build .#ghc902.root
    check 'result/bin/root' 'string' 'Running the main package produced the wrong output'

  '';
}
