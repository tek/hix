{
  updateLock = false;

  source = ''
  step git init -q

  step_nix run path:$hix_dir#bootstrap

  output_match 'success'
  step_run ghci -p red-panda -t main <<< :quit
  '';
}
