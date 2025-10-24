{
  description = "hix test project";

  inputs.hix.url = "HIX";

  outputs = { hix, ... }: hix.lib._hix_test ({config, pkgs, util, ...}: let

    dummy = name: util.zscriptBin name ''
    print 'exe ${name}'
    '';

  in {

    envs.dev = {
      buildInputs = _: [(dummy "env-input")];
    };

    commands.inputs = {
      expose = true;
      buildInputs = [(dummy "command-input")];
      command = util.script "inputs" ''
      env-input
      command-input
      '';
    };

  });
}
