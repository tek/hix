{pkgs, ...}:
{
  source = ''
    cd ./root
    flake_update

    check_match 'nix run .#show-overrides -- dev' 'Hackage: 1.0.0'

    version=$(nix eval .#stm-chans-version.${pkgs.system})
    if [[ $version != '"2.0.0"' ]]
    then
      fail "stm-chans version override in 'root' doesn't supersede the one from 'dep1' (is $version)."
    fi

    nix build .#root.min
    nix build
    output=$(result/bin/run)
    if [[ $output != 'success66' ]]
    then
      fail "Running the main package produced the wrong output:\n$output"
    fi

    step_run gen-cabal-quiet

    cabal_update()
    {
      output_ignore
      error_ignore # Somehow bash complains about the forced locale not existing 🙄
      step_develop cabal update
    }

    if_ci cabal_update
    error_match '3 files worked'
    step_run hls
  '';
}
