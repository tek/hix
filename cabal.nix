{
  packages,
  pkgs,
}:
let
  bumpVersion = desc: file: ''
    current=$(sed -n 's/^version: \(.*\)/\1/p' ${file})
    if [[ -z $current ]]
    then
      print ">>> ERROR: Could not find current version in ${file}."
      exit
    fi
    print ">>> Current version: $current"
    print -n ">>> Please enter new version (empty to keep): "
    read new_version
    version=''${new_version:-$current}
    print -n ">>> ${desc} version $version? [yN] "
    read -q decision
    print ""
    if [[ $decision != 'y' ]]
    then
      print ">>> Aborting."
      exit
    fi
    if [[ -n $new_version ]]
    then
      sed -i "s/^version: .*/version: $new_version/" ${file}
      nix run '.#hpack'
      git add ${file}
      git add packages/*/*.cabal
    fi
  '';

  tagFragment = ''
    if [[ -n $version ]]
    then
      git tag -m "Release $version" "v$version"
    fi
  '';

  upload = { tag, desc, extra ? "" }: { name, versionFile ? null }:
    let
      buildDir = "/tmp/${name}-build";
      buildDirOption = "--builddir ${buildDir}";
      cabal = cmd: "cabal ${cmd} ${buildDirOption}";
    in
      pkgs.writeScript "cabal-upload-candidates" ''
        #!${pkgs.zsh}/bin/zsh
        setopt err_exit
        nix -L flake check
        rm -rf ${buildDir}
        mkdir -p ${buildDir}
        ${if isNull versionFile then "" else bumpVersion desc versionFile}
        ${cabal "v2-build"} all
        ${cabal "v2-sdist"} all
        for pkg in ${buildDir}/sdist/*
        do
          cabal upload ${extra} $pkg
        done
        ${cabal "v2-haddock"} --enable-documentation --haddock-for-hackage all
        for pkg in ${buildDir}/*docs.tar.gz
        do
          cabal upload -d ${extra} $pkg
        done
        if [[ -n $new_version ]]
        then
          git commit -m "v$new_version"
        fi
        ${if tag then tagFragment else ""}
      '';

  uploadApp = config: args:
  pkgs.writeScript "cabal-upload-candidates-app"  ''
    #!/usr/bin/env zsh
    nix develop -c ${upload config args}
  '';
in {
  candidates = uploadApp { tag = false; desc = "Upload candidates for"; };
  release = uploadApp { tag = true; desc = "Release"; extra = "--publish"; };
}
