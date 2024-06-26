{...}:
{
  test = builtins.toFile "basic-test" ''
    cd ./root
    flake_update
    nix run .#gen-cabal-quiet

    target='[ "ghc90-root" "ghc92-root" "ghc94-root" "ghc96-root" "root" ]'
    check 'nix eval .#checks.x86_64-linux --apply builtins.attrNames' $target 'Checks are wrong'

    target='[ "default" "ghc90-root" "ghc92-root" "ghc94-root" "ghc96-root" "min" "profiled" "root" "static" ]'
    check 'nix eval .#packages.x86_64-linux --apply builtins.attrNames' $target 'Packages are wrong'

    check 'nix develop .#ghc94 -c ghc --version' 'The Glorious Glasgow Haskell Compilation System, version 9.4.8' 'Version output wrong'

    nix build .#ghc92-root
    check 'result/bin/root' 'string' 'Running the main package produced the wrong output'

    target='fourmolu 0.14.0.0
using ghc-lib-parser 9.6.5.20240423'
    check 'nix develop -c fourmolu --version' $target 'Fourmolu version output wrong'
  '';
}
