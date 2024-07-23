{pkgs, ...}:
{
  test = pkgs.writeText "bootstrap-test" ''
    cd ./root
    git init -q
    ${pkgs.nix}/bin/nix run path:$hix_dir#bootstrap
    check_match '${pkgs.nix}/bin/nix run .#ghci -- -p red-panda -t main <<< :quit' 'success'
  '';
}
