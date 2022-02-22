{ lib, hsLib, ... }: drv:
with lib;
let

  forHackage = d: hsLib.overrideCabal d (old: {
    doHaddock = true;
    haddockFlags = old.haddockFlags or [] ++ ["--for-hackage"];
  });

  addRelease = { pname, version, outputs, ...}:
  let
    ident = "${pname}-${version}";
    docname = "${ident}-docs";
  in {
    outputs = outputs ++ ["haddock" "sdist"];

    postInstall = ''
      ./Setup sdist
      mkdir -p $haddock
      tar --format=ustar -czf $haddock/${docname}.tar.gz -C dist/doc/html ${docname}
      mkdir -p $sdist
      mv dist/${ident}.tar.gz $sdist/
    '';
  };

in
  overrideDerivation (forHackage drv) addRelease
