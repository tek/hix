{util}: let

  inherit (util) lib build;
  inherit (lib) types;

  extendsDefault = lib.mkOverride 900;

  envDefault = lib.mkOverride 800;

  extendsConfig = {remove ? [], attr, options, target}: let
    default = util.config.${attr}.${target};
    extend = optionName: option: let
      push =
        if lib.isType "attrsOf" option
        then util.mapValues
        else lib.id;
    in push extendsDefault default.${optionName};
    allowed = util.removeKeys (remove ++ ["name" "extends"]) options;
  in
  lib.mkIf (target != null) (lib.mapAttrs extend allowed);

  extendsOption = {attr, type}: lib.mkOption {
    description = ''
    The name of another entry in [](#opt-general-${attr}) whose config values will be used as defaults for this one.
    '';
    type = types.nullOr type;
    default = "default";
  };

  extensibleName = {name, extends, extender}: let

    actual =
      if extender != null
      then
      if extends != null && extends != "default"
      then "${extender}-extends-${extends}"
      else extender
      else name
      ;

  in lib.mkIf (name != null || extender != null) (lib.mkDefault actual);

  extensibleModule = {
    id,
    attr ? "${id}s",
    type ? util.types.ref.${id},
    name,
    config,
    options,
    extraConfig ? {},
    remove ? [],
  }: {

    options = options // {

      name = lib.mkOption {
        description = ''
        The name of this configuration is derived from the attribute when it's defined in [](#opt-general-${attr}), and
        from the containing module if it is extended.
        Should not be overridden.
        '';
        type = types.str;
      };

      extends = extendsOption { inherit attr type; };

      extender = util.maybeOption types.str {
        description = ''
        The name of the containing module if this module is extended.
        '';
      };

    };

    config =
      lib.mkMerge [
        (extendsConfig { inherit attr options remove; target = config.extends; })
        {
          name = extensibleName { inherit name; inherit (config) extends extender; };
        }
        extraConfig
      ];

  };

  extensibleOption = {module, type, attr, desc, extender, ref ? true}: let

    moduleType = types.submoduleWith { modules = [module { inherit extender; }]; };

    entryDesc = "an entry in [](#opt-general-${attr}) ${desc}";

    defaultExplanation = ''
    options will default to the config in `${attr}.default`; or if the option `extends` is set in the submodule, the
    config in `${attr}` by that name
    '';

  in if ref
  then lib.mkOption {
    description = ''
    The name of ${entryDesc}.
    Alternatively, this may be specified as submodule configuration of that type, in which case all
    ${defaultExplanation}.
    '';
    type = types.either type moduleType;
    default = "default";
  }
  else lib.mkOption {
    description = ''
    An submodule extending ${entryDesc}.
    All ${defaultExplanation}.
    '';
    type = moduleType;
    default = {};
  };

  resolveExtensibleModule = type: spec: let
    api = build.${type};
  in
  if builtins.isString spec
  then api.${spec}
  else api spec;

in {
  inherit
  extendsDefault
  envDefault
  extendsConfig
  extendsOption
  extensibleModule
  extensibleOption
  resolveExtensibleModule
  ;
}
