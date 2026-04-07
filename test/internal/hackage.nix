{util}: let

  inherit (util) pkgs;

in {

  # TODO Can't we set up the test user in the integration app?
  withServer = {cabalConf ? true}: main: let

    scoped = ''
    setopt local_options local_traps err_return
    local hackage_port=""

    step nix build path:$hix_dir#env.integration-exe.integration

    output_ignore
    error_ignore
    step_develop which cabal

    systemd-run --quiet --user --collect --unit=$unit --property=Type=exec \
      nix run path:$hix_dir#integration-hackage -- --port-file $port_file
    trap 'hackage_quit 0' EXIT
    trap 'trap - EXIT; hackage_quit 130' INT
    for i in {1..120}
    do
      if [[ -f $port_file ]]
      then
        hackage_port=$(<$port_file)
        break
      fi
      sleep 0.5
    done

    describe 'Validate Hackage port'
    diagnostics _hackage_unit_status
    step _hackage_port_discovered

    local _hackage_url="http://localhost:$hackage_port"
    local _hackage_user_url="http://test@localhost:$hackage_port"

    xh post --quiet --follow --auth admin:admin $_hackage_url/users/ username==test password==test repeat-password==test
    xh put --quiet --follow --auth admin:admin $_hackage_url/packages/uploaders/user/test

    fetch_hackage()
    {
      xh get --follow --auth test:test "$_hackage_url/$1"
    }

    fetch_cabal()
    {
      fetch_hackage package/''${1}/revision/''${2:-0}.cabal
    }

    ${main}
    '';

  in {

    path = [pkgs.xh pkgs.which];

    source = ''
    port_file="$test_base/port"
    unit="hackage-$RANDOM"

    hackage_quit()
    {
      systemctl --quiet --user stop $unit
      return $1
    }

    _hackage_port_discovered()
    {
      [[ -n $hackage_port ]]
    }

    _hackage_unit_status()
    {
        message 'Unit status:'
        _hix_redirect systemctl --user status $unit
    }

    hackage_scope()
    {
      ${scoped}
    }

    hackage_scope
    '';

  };

}
