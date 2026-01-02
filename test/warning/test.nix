{lib, util, ...}: let

  warningTest = line1: lines: let

    new = util.unlines (["evaluation warning: ${line1}"] ++ map (l: if l == "" then "" else "                    ${l}") (util.lines lines));

  in ''
    error_exact "\
  ${new}"
  '';

  evalTests = lib.optionalString (lib.versionAtLeast builtins.nixVersion "2.19") ''
  ${warningTest
  ''The option 'packages.*.subpath' is deprecated in favor of 'project.packages.*.path'.''
  ''
  For instructions on how to access 'project', see [https://hix.how#intermediate-outputs].
  Disable this warning by setting 'ui.warnings.keys.\\\"deprecated.option.packages.subpath\\\" = false;${"'"}''}

  output_exact '"."'
  diff_ignore_trailing_space
  step_eval package-subpath

  ${warningTest
  ''The option 'envs.dev.ghc.version' is deprecated in favor of 'envs.*.toolchain.version'.''
  ''
  You can find more information about customizing compilers and package sets at [https://hix.how#ghc].
  Disable this warning by setting 'ui.warnings.keys.\\\"deprecated.option.env.ghc\\\" = false;${"'"}''}

  output_exact '"9.10.3"'
  diff_ignore_trailing_space
  step_eval ghc-version

  ${warningTest
  ''The configuration for 'envs.duplicate' specifies both 'compiler' and 'package-set.compiler.source'.''
  ''
  The former will be ignored.

  Disable this warning by setting 'ui.warnings.keys.\\\"env.duplicate-compiler\\\" = false;${"'"}''}

  output_exact '"9.8.4"'
  diff_ignore_trailing_space
  step_eval project.config.envs.duplicate.toolchain.version

  error_exact "\
         error: The option 'envs.ghc-overrides.ghc.overrides' is deprecated.
         It is superseded by 'package-sets.*.overrides', possibly with changed semantics.

         You can find more information about customizing compilers and package sets at [https://hix.how#ghc]."
  exit_code 1
  preproc_error "take_end 5 | sed '$ { /^$/ d }' | sed '1 { /^$/ d }'"
  step_eval ghc-overrides
  '';

in {
  source = ''
  ${evalTests}

  error_exact "\
  [1m[35m>>>[0m[0m [31mThe environment [33munexposed[0m[31m is configured not to be exposed at this \
  flake output.[0m
  [1m[35m>>>[0m[0m [31mYou can enable it by setting [34menvs.unexposed.expose.shell = \
  true;[0m[31m[0m
  [1m[35m>>>[0m[0m [31mYou can use this shell anyway with [34mnix develop .#env.unexposed.shell[0m[31m[0m"
  exit_code 1
  step_develop_in unexposed echo 'do not execute'

  error_exact "\
  [1m[35m>>>[0m[0m [31mThe environment [33msystems[0m[31m is disabled because \
  [35menvs.systems.systems[0m[31m is set and does not contain the current system, \
  [32mx86_64-linux[0m[31m.[0m"
  exit_code 1
  step_develop_in systems echo 'do not execute'

  error_exact "\
  [1m[35m>>>[0m[0m [31mThe environment [33mdisabled[0m[31m is disabled because the option \
  [35menvs.disabled.enable[0m[31m is set to [34mfalse[0m[31m.[0m"
  exit_code 1
  step_develop_in disabled echo 'do not execute'
  ''
  ;
}
