{pkgs}: {

  # TODO Can't we set up the test user in the integration app?
  withServer = main: {

    path = [pkgs.xh];

    source = ''
    port_file="$test_base/port"
    hackage_log="$test_base/hackage.log"

    hackage_quit()
    {
      if (( ''${+integration_pid} == 1 )) kill $integration_pid
      return $1
    }

    hackage_run()
    {
      nix run path:$hix_dir#integration-hackage -- --port-file $port_file &> $hackage_log
    }

    hackage_scope()
    {
      setopt local_options local_traps err_return
      step nix build path:$hix_dir#env.integration.integration
      coproc hackage_run
      integration_pid=$!
      trap 'hackage_quit 0' EXIT
      trap 'trap - EXIT; hackage_quit 130' INT
      for i in {1..120}
      do
        if [[ -f $port_file ]]
        then
          port=$(<$port_file)
          break
        fi
        sleep 0.5
      done
      if [[ -z $port ]]
      then
        error_message "Port file didn't appear. Output:"
        cat $hackage_log
        return 1
      fi

      local _hackage_url="http://localhost:$port"
      local _hackage_user_url="http://test@localhost:$port"

      local CABAL_CONFIG=$test_base/cabal.conf
      export CABAL_CONFIG

      cat > $CABAL_CONFIG << EOF
    remote-repo: hackage:$_hackage_url
    username: test
    password: test
    EOF

      xh post --quiet --follow --auth admin:admin $_hackage_url/users/ username==test password==test repeat-password==test
      xh put --quiet --follow --auth admin:admin $_hackage_url/packages/uploaders/user/test

    ${main}
    }

    hackage_scope
    '';

  };

  releaseFixtures = ''
  doc_target() {
    local v=''${2:-0.1.0.0}
    cat <<EOF
  {"doc":true,"path":"/nix/store/hash-root-''${v}-haddock/root-''${v}-docs.tar.gz","publish":$1}
  EOF
  }
  src_target() {
    local v=''${2:-0.1.0.0}
    cat <<EOF
  {"doc":false,"path":"/nix/store/hash-root-''${v}-sdist/root-''${v}.tar.gz","publish":$1}
  EOF
  }
  release_target="$(src_target true)
  $(doc_target true)"

  candidates_target="$(src_target false)
  $(doc_target false)"

  run() {
    nix run $* | sed 's#/nix/store/[^-]\+-##g'
  }

  pristine_version='0.1.0.0'
  pristine_target="$(src_target true $pristine_version)
  $(doc_target true $pristine_version)"
  version='1.0.0.0'
  version_target="$(src_target true $version)
  $(doc_target true $version)"
  manual_version="2.0.0.0"
  manual_target="$(src_target true $manual_version)
  $(doc_target true $manual_version)"
  '';

}
