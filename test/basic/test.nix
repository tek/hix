{
  genCabal = true;

  source = ''
    output_exact '[ "dev-dep" "dev-root" "ghc910-dep" "ghc910-root" "ghc94-dep" "ghc94-root" "ghc96-dep" "ghc96-root" "ghc98-dep" "ghc98-root" ]'
    step_eval checks.x86_64-linux --apply builtins.attrNames

    output_exact '[ "default" "dep" "min" "musl" "profiled" "root" "static" ]'
    step_eval packages.x86_64-linux --apply builtins.attrNames

    output_exact 'The Glorious Glasgow Haskell Compilation System, version 9.8.4'
    step_develop_in ghc98 ghc --version

    step_build ghc96.root

    output_exact 'string'
    step result/bin/root

    output_exact 'fourmolu 0.19.0.0
    using ghc-lib-parser 9.12.2.20250421'
    step_develop fourmolu --version
  '';
}
