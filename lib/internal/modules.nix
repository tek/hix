{util}: let

  inherit (util) lib build internal;
  inherit (lib) types;

  extendsDefault = lib.mkOverride 900;

  envDefault = lib.mkOverride 800;

  pushableValue = value:
  lib.isAttrs value
  &&
  !(
    builtins.isPath value
    ||
    lib.isDerivation value
    ||
    lib.isStorePath value
  )
  ;

  extendSpec = pushAttrs: name: value: let
    next = pushAttrs.${name} or null;
  in
  if next == null
  then extendsDefault value
  else
  if pushableValue value
  then lib.mapAttrs (extendSpec next) value
  else value
  ;

  extendsConfig = {remove ? [], attr, options, target, pushDefault}: let
    default = util.config.${attr}.${target};

    extend = pushAttrs: optionName: option: let
      value = default.${optionName};

      next = pushAttrs.${optionName} or null;

    in if
    pushableValue value
    &&
    (
      option.type.name == "attrsOf"
      ||
      option.type.name == "submodule"
      ||
      pushAttrs ? ${optionName}
    )
    then lib.mapAttrs (extendSpec next) value
    else extendsDefault value;

    allowed = util.removeKeys (remove ++ ["name" "extends"]) options;

  in lib.mkIf (target != null) (lib.mapAttrs (extend pushDefault) allowed);

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
    pushDefault ? {},
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
        (extendsConfig { inherit attr options remove pushDefault; target = config.extends; })
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

  deprecatedOptionDefined = {loc, extra ? null, replacement ? null}: let
    replacementMessage = "It is superseded by '${replacement}', possibly with changed semantics.";
  in util.unlines (
    ["The option '${lib.showOption loc}' is deprecated."]
    ++
    lib.optional (replacement != null) replacementMessage
    ++
    lib.optional (extra != null) extra
  );

  deprecated = {type, key, replacement ? null, definitionReplacement ? null, extra ? null}: type // {
    name = "deprecated ${type.name}";
    merge = loc: defs:
      if lib.length defs == 1
      then internal.warn.deprecatedOptionReadOnly {
        inherit replacement extra key;
        option = lib.showOption loc;
      } (lib.head defs).value
      else throw (deprecatedOptionDefined { inherit loc extra; replacement =  definitionReplacement; })
      ;
  };

  deprecatedOption = {
    type,
    key,
    replacement ? null,
    definitionReplacement ? replacement,
    extra ? null,
    description ? "Deprecated.",
  }: lib.mkOption {
    inherit description;
    type = deprecated { inherit type key replacement definitionReplacement extra; };
  };

in {
  inherit
  extendsDefault
  envDefault
  extendsConfig
  extendsOption
  extensibleModule
  extensibleOption
  resolveExtensibleModule
  deprecatedOption
  ;
}
