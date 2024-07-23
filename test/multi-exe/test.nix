{...}:
{
  test = builtins.toFile "multi-exe-test" ''
    cd ./root
    flake_update

    target='[ "bump" "exe1" "exe2" "exe3" "ghci" "ghcid" "hls" "lower" "pkg1" "pkg1_exe1" "pkg1_main" "pkg2" "pkg2_exe2" "program" "run" "type" ]'
    check 'nix eval .#apps.x86_64-linux.env.dev --apply builtins.attrNames' $target

    check 'nix run .#env.dev.exe1' 'pkg2'
    check 'nix run .#env.dev.exe2' 'pkg1'
    check 'nix run .#env.dev.exe3' 'pkg1'

    target='[ "bump" "exe1" "exe3" "ghci" "ghcid" "hls" "lower" "pkg2" "pkg2_exe2" "program" "run" "type" ]'
    check 'nix eval .#apps.x86_64-linux.env.p2 --apply builtins.attrNames' $target

    check 'nix run .#env.p2.exe3' 'pkg2'

    target='[ "default" "min" "musl" "pkg1" "pkg2" "profiled" "static" ]'
    check 'nix eval .#packages.x86_64-linux --apply builtins.attrNames' $target

    target='[ "dev-pkg1" "dev-pkg2" "ghc90-pkg1" "ghc90-pkg2" "ghc90-pkg4" "ghc92-pkg1" "ghc92-pkg2" "ghc92-pkg4" "ghc94-pkg1" "ghc94-pkg2" "ghc94-pkg4" "ghc96-pkg1" "ghc96-pkg2" "ghc96-pkg4" ]'
    check 'nix eval .#checks.x86_64-linux --apply builtins.attrNames' $target

    target='[ "appimage" "exe2" "exe3" "pkg1" "pkg1_exe1" "pkg1_main" "pkg2" "pkg3" ]'
    check 'nix eval .#legacyPackages.x86_64-linux.dev --apply builtins.attrNames' $target
  '';
}
