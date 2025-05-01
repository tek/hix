{util}: let

  inherit (util) config lib;
  inherit (util.console) chevrons bold;
  inherit (util.console.s) colors;

  inherit (config.ui.warnings) all keys;

  colorIf = allow: render: text: if allow then render text else "'${text}'";

  enabled = key: keys.${key} or all;

  warnWith = color: handler: key: message:
  handler (enabled key) ''
  ${message}
  Disable this warning by setting ${colorIf color (lib.pipe [colors.yellow bold]) ''ui.warnings.keys."${key}" = false;''}'';

  warnEval = pred: warnWith false (enabled: lib.warnIf (pred && enabled));

  deprecatedWith = {handler, key, desc, replacement, color ? true, extra ? null}: let
    extraAdjusted = lib.optionalString (extra != null) " ${extra}";
    suggestion = if replacement == null then "" else " in favor of ${colorIf color colors.blue replacement}";
    message = "${desc} is deprecated${suggestion}.${extraAdjusted}";
  in warnWith color handler "deprecated.${key}" message;

  # TODO does "$(cat ${warning})" cause problems with quotes in the file?
  scriptHandler = exe: isEnabled: message: let
    warning = config.pkgs.writeText "hix-warning" ''
    ${chevrons} Warning: ${message}
    '';
    wrapped =
      if isEnabled
      then util.script "hix-warn-wrapper" ''
      echo -e "$(cat ${warning})"
      exec ${exe}
      ''
      else exe
      ;
  in wrapped;

  appHandler = exe: isEnabled: message:
  util.app (scriptHandler exe isEnabled message);

  legacyAppHandler = exe: isEnabled: message:
  util.ensureLegacyApp "deprecated-app" (scriptHandler exe isEnabled message);

  deprecatedEval = args: deprecatedWith ({ color = false; handler = lib.warnIf; } // args);

  deprecatedOptionReadOnly = { key, option, replacement, extra ? null }:
  deprecatedEval {
    key = "option.${key}";
    desc = "The option '${option}'";
    inherit replacement extra;
  };

  appArgs = name: replacement: {
    key = "app.${name}";
    desc = "The app ${colors.blue ".#${name}"}";
    inherit replacement;
  };

  deprecatedApp = name: replacement: exe:
  deprecatedWith (appArgs name replacement // { handler = appHandler exe; });

  deprecatedLegacyApp = name: replacement: exe:
  deprecatedWith (appArgs name replacement // { handler = legacyAppHandler exe; });

in {
  inherit
  warnWith
  warnEval
  deprecatedWith
  deprecatedEval
  deprecatedOptionReadOnly
  deprecatedApp
  deprecatedLegacyApp
  ;
}
