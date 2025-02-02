{...}:
{
  source = ''
    cd ./root
    flake_update

    output_exact '{"bump":"/nix/store/hash-bump","exe1":"/nix/store/hash-exe1-0.1.0.0","exe2":"/nix/store/hash-exe2-0.1.0.0","exe3":"/nix/store/hash-exe3-0.1.0.0","executables":{"exe1":"/nix/store/hash-exe1-0.1.0.0","exe2":"/nix/store/hash-exe2-0.1.0.0","exe3":"/nix/store/hash-exe3-0.1.0.0","pkg1":"/nix/store/hash-pkg1_main-0.1.0.0","pkg1_exe1":"/nix/store/hash-pkg1_exe1-0.1.0.0","pkg1_main":"/nix/store/hash-pkg1_main-0.1.0.0","pkg2":"/nix/store/hash-pkg2-0.1.0.0","pkg2_exe2":"/nix/store/hash-pkg2_exe2-0.1.0.0","pkg3":"/nix/store/hash-pkg3-0.1.0.0"},"ghci":"/nix/store/hash-ghci","ghcid":"/nix/store/hash-ghcid","hls":"/nix/store/hash-hls","lower":"/nix/store/hash-lower","musl":"/nix/store/hash-pkg1-0.1.0.0","pkg1":"/nix/store/hash-pkg1-0.1.0.0","pkg1_exe1":"/nix/store/hash-pkg1_exe1-0.1.0.0","pkg1_main":"/nix/store/hash-pkg1_main-0.1.0.0","pkg2":"/nix/store/hash-pkg2-0.1.0.0","pkg2_exe2":"/nix/store/hash-pkg2_exe2-0.1.0.0","pkg3":"/nix/store/hash-pkg3-0.1.0.0","release":"/nix/store/hash-pkg1-0.1.0.0","run":"/nix/store/hash-run","static":"/nix/store/hash-pkg1-static-x86_64-unknown-linux-musl-0.1.0.0"}'
    preproc_output sub_store_hash
    step_eval legacyPackages.x86_64-linux.env.dev --json

    output_exact 'pkg2'
    step_run dev.exe1

    output_exact 'pkg1'
    step_run dev.exe2

    output_exact 'pkg1'
    step_run dev.exe3

    output_exact '[ "bump" "exe1" "exe3" "executables" "ghci" "ghcid" "hls" "lower" "musl" "pkg2" "pkg2_exe2" "release" "run" "static" ]'
    step_eval legacyPackages.x86_64-linux.env.p2 --apply builtins.attrNames

    output_exact '[ "dep-versions" "exe1" "exe3" "executables" "musl" "pkg2" "pkg2_exe2" "release" "static" ]'
    step_eval legacyPackages.x86_64-linux.p2 --apply builtins.attrNames

    output_exact 'pkg2'
    step_run p2.exe3

    output_exact '[ "default" "min" "musl" "pkg1" "pkg2" "profiled" "static" ]'
    step_eval packages.x86_64-linux --apply builtins.attrNames

    output_exact '[ "dev-pkg1" "dev-pkg2" "ghc90-pkg1" "ghc90-pkg2" "ghc90-pkg4" "ghc92-pkg1" "ghc92-pkg2" "ghc92-pkg4" "ghc94-pkg1" "ghc94-pkg2" "ghc94-pkg4" "ghc96-pkg1" "ghc96-pkg2" "ghc96-pkg4" ]'
    step_eval checks.x86_64-linux --apply builtins.attrNames

    output_exact '[ "dep-versions" "exe1" "exe2" "exe3" "executables" "musl" "pkg1" "pkg1_exe1" "pkg1_main" "pkg2" "pkg2_exe2" "release" "static" ]'
    step_eval legacyPackages.x86_64-linux.dev --apply builtins.attrNames

    output_exact '[ "__hix-internal__" "build" "bump" "cmd" "config" "dev" "env" "ghc" "ghc0" "ghc90" "ghc92" "ghc94" "ghc96" "hackage" "hpack" "hpack-quiet" "lower" "managed" "overrides" "p2" "pkg1" "pkg2" "pkgs" "show-config" ]'
    step_eval legacyPackages.x86_64-linux --apply builtins.attrNames
  '';
}
