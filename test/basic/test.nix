{ pkgs }:
{
  test = builtins.toFile "basic-test" ''
    cd ./root
    nix flake update

    target='[ "ghc810-root" "ghc90-root" "ghc92-root" "ghc94-root" "root" ]'
    check 'nix eval .#checks.x86_64-linux --apply builtins.attrNames' $target 'Checks are wrong'

    target='[ "default" "ghc810-root" "ghc90-root" "ghc92-root" "ghc94-root" "min" "root" "static" ]'
    check 'nix eval .#packages.x86_64-linux --apply builtins.attrNames' $target 'Packages are wrong'

    check 'nix develop .#ghc94 -c ghc --version' 'The Glorious Glasgow Haskell Compilation System, version 9.4.5'

    nix build .#ghc92-root
    check 'result/bin/root' 'string' 'Running the main package produced the wrong output'
  '';
}