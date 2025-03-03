{
  source = ''
    output_exact '{"bump":"/nix/store/hash-managed-disabled","dep-versions":"/nix/store/hash-print-dep-versions","exe1":"/nix/store/hash-exe1-0.1.0.0","exe2":"/nix/store/hash-exe2-0.1.0.0","exe3":"/nix/store/hash-exe3-0.1.0.0","executables":{"exe1":"/nix/store/hash-exe1-0.1.0.0","exe2":"/nix/store/hash-exe2-0.1.0.0","exe3":"/nix/store/hash-exe3-0.1.0.0","pkg1":"/nix/store/hash-pkg1_main-0.1.0.0","pkg1_exe1":"/nix/store/hash-pkg1_exe1-0.1.0.0","pkg1_main":"/nix/store/hash-pkg1_main-0.1.0.0","pkg2":"/nix/store/hash-pkg2-0.1.0.0","pkg2_exe2":"/nix/store/hash-pkg2_exe2-0.1.0.0","pkg3":"/nix/store/hash-pkg3-0.1.0.0"},"ghci":"/nix/store/hash-ghci","ghcid":"/nix/store/hash-ghcid","hls":"/nix/store/hash-hls","lower":"/nix/store/hash-managed-disabled","musl":"/nix/store/hash-pkg1-0.1.0.0","pkg1":"/nix/store/hash-pkg1-0.1.0.0","pkg1_exe1":"/nix/store/hash-pkg1_exe1-0.1.0.0","pkg1_main":"/nix/store/hash-pkg1_main-0.1.0.0","pkg2":"/nix/store/hash-pkg2-0.1.0.0","pkg2_exe2":"/nix/store/hash-pkg2_exe2-0.1.0.0","pkg3":"/nix/store/hash-pkg3-0.1.0.0","release":"/nix/store/hash-pkg1-0.1.0.0","run":"/nix/store/hash-run","shell":"/nix/store/hash-dev","static":"/nix/store/hash-pkg1-static-x86_64-unknown-linux-musl-0.1.0.0"}'
    preproc_output sub_store_hash
    step_eval legacyPackages.x86_64-linux.env.dev --json

    output_exact 'pkg2'
    step_run dev.exe1

    output_exact 'pkg1'
    step_run dev.exe2

    output_exact 'pkg1'
    step_run dev.exe3

    output_exact '[ "bump" "dep-versions" "exe1" "exe3" "executables" "ghci" "ghcid" "hls" "lower" "musl" "pkg2" "pkg2_exe2" "release" "run" "shell" "static" ]'
    step_output_names legacyPackages.x86_64-linux.env.p2

    output_exact '[ "dep-versions" "exe1" "exe3" "executables" "musl" "pkg2" "pkg2_exe2" "release" "shell" "static" ]'
    step_output_names legacyPackages.x86_64-linux.p2

    output_exact 'pkg2'
    step_run p2.exe3

    output_exact '[ "default" "min" "musl" "pkg1" "pkg2" "profiled" "static" ]'
    step_output_names packages.x86_64-linux

    output_exact '[ "dev-pkg1" "dev-pkg2" "ghc910-pkg1" "ghc910-pkg2" "ghc910-pkg4" "ghc94-pkg1" "ghc94-pkg2" "ghc94-pkg4" "ghc96-pkg1" "ghc96-pkg2" "ghc96-pkg4" "ghc98-pkg1" "ghc98-pkg2" "ghc98-pkg4" ]'
    step_output_names checks.x86_64-linux

    output_exact '[ "dep-versions" "exe1" "exe2" "exe3" "executables" "musl" "pkg1" "pkg1_exe1" "pkg1_main" "pkg2" "pkg2_exe2" "release" "shell" "static" ]'
    step_output_names legacyPackages.x86_64-linux.dev

    output_exact '[ "__hix-internal__" "build" "bump" "cmd" "dev" "env" "ghc910" "ghc94" "ghc96" "ghc98" "hackage" "hpack" "hpack-quiet" "lower" "managed" "overrides" "p2" "pkg1" "pkg2" "project" ]'
    step_output_names legacyPackages.x86_64-linux
  '';
}
