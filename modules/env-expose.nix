{util}: let

  inherit (util.lib) mkOption types;

in
{...}: {

  options = {

    shell = mkOption {
      description = "Whether to expose this env in [](#opt-general-outputs.devShells).";
      type = types.bool;
      default = true;
    };

    envKeyed = mkOption {
      description = "Whether to expose packages in this env as a root output named after the env (like `profiled`).";
      type = types.bool;
      default = false;
    };

  };

}
