{lib, util, ...}: let

  warningTest = message: let

    lines = util.lines message;

    line1 = lib.head lines;
    rest = lib.tail lines;

    old = util.unlines (["trace: evaluation warning: ${line1}"] ++ rest);

    new = util.unlines (["evaluation warning: ${line1}"] ++ map (l: "                     ${l}") rest);

  in ''
  if [[ $(nix --version) =~ '2\.18' ]]
  then
    error_exact "\
  ${old}"
  else
    error_exact "\
  ${new}"
  fi
  '';

in {
  source = ''
  ${warningTest ''
  The option 'packages.*.subpath' is deprecated in favor of 'project.packages.*.path'. 
  For instructions on how to access 'project', see [https://tryp.io/hix#intermediate-outputs].
  Disable this warning by setting 'ui.warnings.keys.\\\"deprecated.option.packages.subpath\\\" = false;''\'''}

  output_exact '"."'
  step_eval package-subpath

  ${warningTest ''
  The option 'envs.dev.ghc.version' is deprecated. 
  You can find more information about customizing compilers and package sets at [https://hix.how#ghc].
  Disable this warning by setting 'ui.warnings.keys.\\\"deprecated.option.env.ghc\\\" = false;''\'''}

  output_exact '"9.8.4"'
  step_eval ghc-version

  ${warningTest ''
  The configuration for 'envs.duplicate' specifies both 'compiler' and 'package-set.compiler.source'.
  The former will be ignored.

  Disable this warning by setting 'ui.warnings.keys.\\\"env.duplicate-compiler\\\" = false;''\'''}

  output_exact '"9.8.4"'
  step_eval project.config.envs.duplicate.toolchain.version
  '';
}
