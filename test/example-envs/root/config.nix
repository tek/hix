{
  envs.example = {

    compiler = "ghc910";

    shellTools = pkgs: [pkgs.socat];

    services.postgres = {
      enable = true;
      config = { name = "test-db"; };
    };

    expose.shell = true;

  };
}
