{ pkgs }:
{
  test = builtins.toFile "auto-5-test" ''
    cd ./root
    nix flake update

    target='[ "ghc810-root" "ghc90-root" "ghc92-root" "ghc94-root" "root" ]'
    check 'nix eval .#checks.x86_64-linux --apply builtins.attrNames' $target 'Checks are wrong'

    target='[ "default" "ghc810-root" "ghc90-root" "ghc92-root" "ghc94-root" "min" "root" ]'
    check 'nix eval .#packages.x86_64-linux --apply builtins.attrNames' $target 'Packages are wrong'

    nix build .#ghc90-root
    check 'result/bin/root' 'string' 'Running the main package produced the wrong output'

  '';
}
