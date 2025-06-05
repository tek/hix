{util}: let

  hookUpdateChangelogs =
    util.hixScript "release-hook-update-changelogs" { path = pkgs: [pkgs.gnused pkgs.fd]; } ''
    if [[ -n $version ]]
    then
      changelogs=$(fd -i 'changelog(\..*)?')
      if [[ -n $changelogs ]]
      then
        sed -i "s/Unreleased/''${version}/" $=changelogs
      fi
    fi
    '';

in {
  inherit hookUpdateChangelogs;
}
