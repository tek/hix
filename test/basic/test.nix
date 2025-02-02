{...}:
{
  test = builtins.toFile "basic-test" ''
    cd ./root
    flake_update
    nix run .#gen-cabal-quiet

    target='[ "dev-dep" "dev-root" "ghc90-dep" "ghc90-root" "ghc92-dep" "ghc92-root" "ghc94-dep" "ghc94-root" "ghc96-dep" "ghc96-root" ]'
    check 'nix eval .#checks.x86_64-linux --apply builtins.attrNames' $target

    target='[ "default" "dep" "min" "musl" "profiled" "root" "static" ]'
    check 'nix eval .#packages.x86_64-linux --apply builtins.attrNames' $target

    check 'nix develop .#ghc94 -c ghc --version' 'The Glorious Glasgow Haskell Compilation System, version 9.4.8'

    nix build .#ghc92.root
    check 'result/bin/root' 'string'

    target='fourmolu 0.14.0.0
    using ghc-lib-parser 9.6.5.20240423'
    check 'nix develop -c fourmolu --version' $target
  '';
}
