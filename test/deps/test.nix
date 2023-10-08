{ pkgs }:
{
  test = builtins.toFile "deps-test" ''
    cd ./root
    flake_update

    check_match 'nix run .#show-overrides -- dev' 'Hackage: 1.0.0' 'show-overrides does not contain "Hackage: 1.0.0"'

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

    nix run .#hls 2>/dev/null
  '';
}
