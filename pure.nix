{
  noOverrides = _: {};

  packagePath = base: pp:
  if builtins.isPath pp
  then pp
  else "${base}/${pp}";
}
