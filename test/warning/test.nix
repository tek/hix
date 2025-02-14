{
  source = ''
  if [[ $(nix --version) =~ '2\.18' ]]
  then
    error_exact "\
  trace: evaluation warning: The option 'packages.*.subpath' is deprecated in favor of 'project.packages.*.path'. 
  For instructions on how to access 'project', see [https://tryp.io/hix#intermediate-outputs].
  Disable this warning by setting 'ui.warnings.keys.\"deprecated.option.packages.subpath\" = false;'"
  else
    error_exact "\
  evaluation warning: The option 'packages.*.subpath' is deprecated in favor of 'project.packages.*.path'.
                      For instructions on how to access 'project', see [https://tryp.io/hix#intermediate-outputs].
                      Disable this warning by setting 'ui.warnings.keys.\"deprecated.option.packages.subpath\" = false;'"
  fi
  output_exact '"."'
  step_eval package-subpath
  '';
}
