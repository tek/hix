{ pkgs }:
{
  test = builtins.toFile "modules-test" ''
    cd ./root
    nix flake update
    nix run .#hpack-verbose

    nix build .#root.min
    nix build
    output=$(result/bin/run)
    if [[ $output != 'success66' ]]
    then
      fail "Running the main package produced the wrong output:\n$output"
    fi

    version=$(nix eval .#stm-chans-version.${pkgs.system})
    if [[ $version != '"2.0.0"' ]]
    then
      fail "stm-chans version override in 'root' doesn't supersede the one from 'dep1' (is $version)."
    fi

    nix build .#compat-902-root

    nix run .#hls 2>/dev/null
  '';
}
