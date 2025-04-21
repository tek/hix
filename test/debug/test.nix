{

  source = ''
  LANG=en_US.UTF-8
  locale
  print $LANGUAGE
  print $LC_ALL
  print $LOCALE_ARCHIVE
  message "Debug test"
  nix run
  '';

}
