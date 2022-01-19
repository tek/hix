{ pkgs }: {
  test = ''
    sed -i "s#HIX#$hix_dir#" */flake.nix
    sed -i "s#BASE#$testdir#" */flake.nix
    cd ./root
    nix flake update
    nix run .#hpack-verbose

    # build main package
    nix build
    output=$(result/bin/run)
    if [[ $output != 'success66' ]]
    then
      fail "Running the main package produced the wrong output:\n$output"
    fi

    # build minimal derivation of main package
    nix build .#root.min
    nix build .#min
    output=$(result/bin/run)
    if [[ $output != 'success66' ]]
    then
      fail "Running the minimized main package produced the wrong output:\n$output"
    fi

    version=$(nix eval .#stm-chans-version.${pkgs.system})
    if [[ $version != '"2.0.0"' ]]
    then
      fail "stm-chans version override in 'root' doesn't supersede the one from 'dep1' (is $version)."
    fi
  '';
}
