{lib, util, ...}: let

  warningTest = line1: lines: let

    new = util.unlines (["evaluation warning: ${line1}"] ++ map (l: "                    ${l}") (util.lines lines));

  in ''
    error_exact "\
  ${new}"
  '';

in {
  source = lib.optionalString (lib.versionAtLeast builtins.nixVersion "2.19") ''
  ${warningTest
  ''The option 'packages.*.subpath' is deprecated in favor of 'project.packages.*.path'.''
  ''
  For instructions on how to access 'project', see [https://tryp.io/hix#intermediate-outputs].
  Disable this warning by setting 'ui.warnings.keys.\\\"deprecated.option.packages.subpath\\\" = false;''\'''}

  output_exact '"."'
  diff_ignore_trailing_space
  step_eval package-subpath

  ${warningTest
  ''The option 'envs.dev.ghc.version' is deprecated.''
  ''
  You can find more information about customizing compilers and package sets at [https://hix.how#ghc].
  Disable this warning by setting 'ui.warnings.keys.\\\"deprecated.option.env.ghc\\\" = false;''\'''}

  output_exact '"9.8.4"'
  diff_ignore_trailing_space
  step_eval ghc-version

  ${warningTest
  ''The configuration for 'envs.duplicate' specifies both 'compiler' and 'package-set.compiler.source'.''
  ''
  The former will be ignored.

  Disable this warning by setting 'ui.warnings.keys.\\\"env.duplicate-compiler\\\" = false;''\'''}

  output_exact '"9.8.4"'
  diff_ignore_trailing_space
  step_eval project.config.envs.duplicate.toolchain.version
  '';
}
