{
  genCabal = true;

  source = ''
    output_exact '[ "dev-dep" "dev-root" "ghc90-dep" "ghc90-root" "ghc92-dep" "ghc92-root" "ghc94-dep" "ghc94-root" "ghc96-dep" "ghc96-root" ]'
    step_eval checks.x86_64-linux --apply builtins.attrNames

    output_exact '[ "default" "dep" "min" "musl" "profiled" "root" "static" ]'
    step_eval packages.x86_64-linux --apply builtins.attrNames

    output_exact 'The Glorious Glasgow Haskell Compilation System, version 9.4.8'
    step_develop_in ghc94 ghc --version

    step_build ghc92.root

    output_exact 'string'
    step result/bin/root

    output_exact 'fourmolu 0.14.0.0
    using ghc-lib-parser 9.6.5.20240423'
    step_develop fourmolu --version
  '';
}
