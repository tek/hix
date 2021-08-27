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

  commitFragment = ''
    git commit --allow-empty -m "v$version"
  '';

  haddockFragment = extra: { buildDir, cabal }: ''
    ${cabal "v2-haddock"} --enable-documentation --haddock-for-hackage all
    for pkg in ${buildDir}/*docs.tar.gz
    do
      cabal upload -d ${extra} $pkg
    done
  '';

  uploadScript = { name, check ? true }: content:
    let
      buildDir = "/tmp/${name}-build";
      cabal = cmd: "cabal ${cmd} --builddir ${buildDir}";
    in
      pkgs.writeScript "cabal-upload" ''
        #!${pkgs.zsh}/bin/zsh
        setopt err_exit
        ${if check then "nix -L flake check" else ""}
        rm -rf ${buildDir}
        mkdir -p ${buildDir}
        ${content { inherit buildDir cabal; }}
      '';

  uploadSource = { tag, commit, desc, extra ? "", check ? true }: { name, versionFile ? null }:
    let script = { buildDir, cabal }: ''
      ${if isNull versionFile then "" else bumpVersion desc versionFile}
      ${cabal "v2-sdist"} all
      for pkg in ${buildDir}/sdist/*
      do
        cabal upload ${extra} $pkg
      done
      ${haddockFragment extra { inherit buildDir cabal; }}
      ${if commit then commitFragment else ""}
      ${if tag then tagFragment else ""}
    '';
    in uploadScript { inherit name check; } script;

  uploadDocs = { name }:
    uploadScript { inherit name; check = false; } (haddockFragment "--publish");

  uploadApp = script:
  pkgs.writeScript "cabal-upload-app"  ''
    #!${pkgs.bash}/bin/bash
    nix develop -c ${script}
  '';
in {
  candidates = args:
    uploadApp (uploadSource { tag = false; commit = false; desc = "Upload candidates for"; check = false; } args);
  release = args:
    uploadApp (uploadSource { tag = true; commit = true; desc = "Release"; extra = "--publish"; } args);
  docs = args:
    uploadApp (uploadDocs args);
}
