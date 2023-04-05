{ pkgs }:
{
  test = builtins.toFile "auto-3-test" ''
    cd ./root
    nix flake update

    nix build
    output=$(result/bin/root)
    if [[ $output != 'string' ]]
    then
      fail "Running the main package with 'build' produced the wrong output:\n$output"
    fi

    output=$(nix run)
    if [[ $output != 'string' ]]
    then
      fail "Running the main package with 'nix run' produced the wrong output:\n$output"
    fi

    check_match 'nix develop -c ghc --version' '9.0.2' 'Devshell GHC version is wrong'

    check_match 'nix develop .#ghc92 -c ghc --version' '9.2.4' 'Devshell GHC version for ghc92 is wrong'

  '';
}
