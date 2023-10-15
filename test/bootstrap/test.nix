{pkgs, ...}:
{
  test = pkgs.writeText "bootstrap-test" ''
    cd ./root
    git init
    check_match '${pkgs.nix}/bin/nix run path:$hix_dir#bootstrap' 'generated red-panda-core' "Bootstrap command didn't generate cabal files"
    check_match '${pkgs.nix}/bin/nix run .#ghci -- -p red-panda -t main <<< :quit' 'success' 'Running tests in generated project failed'
  '';
}
