{
  path = pkgs: [pkgs.jq];

  source = ''
    output_exact ${./dev-outputs.json}
    preproc_output 'sub_store_hash | jq .'
    step_eval legacyPackages.x86_64-linux.env.dev --json

    output_exact 'pkg2'
    step_run dev.exe1

    output_exact 'pkg1'
    step_run dev.exe2

    output_exact 'pkg1'
    step_run dev.exe3

    output_exact '[ "bump" "dep-versions" "exe1" "exe3" "executables" "ghci" "ghcid" "hls" "lower" "maint" "musl" "pkg2" "pkg2_exe2" "release" "revision" "run" "shell" "static" ]'
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

    output_exact '[ "__hix-internal__" "build" "cmd" "dev" "env" "ghc910" "ghc94" "ghc96" "ghc98" "hackage" "hix-build-tools" "hls" "hpack" "hpack-quiet" "lower" "managed" "min" "overrides" "p2" "pkg1" "pkg2" "profiled" "project" ]'
    step_output_names legacyPackages.x86_64-linux

    output_exact '[ "bump" "candidates" "cli" "dep-versions" "docs" "exe1" "exe2" "exe3" "gen" "gen-cabal" "gen-cabal-quiet" "gen-overrides" "gen-quiet" "ghci" "ghcid" "hls" "lower" "maint" "pkg1" "pkg1_exe1" "pkg1_main" "pkg2" "pkg2_exe2" "release" "release-source" "revision" "show-config" "show-overrides" "tags" ]'
    step_output_names apps.x86_64-linux
  '';
}
