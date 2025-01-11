{util}: let

  inherit (util) config lib;
  inherit (util.console) chevrons bold;
  inherit (util.console.s) colors;

  inherit (config.ui.warnings) all keys;

  enabled = key: keys.${key} or all;

  warnWith = handler: key: message:
  handler (enabled key) ''
  ${message}
  Disable this warning by setting ${bold (colors.yellow ''ui.warnings.keys."${key}" = false;'')}
  '';

  deprecatedWith = handler: key: desc: replacement: let
    suggestion = if replacement == null then "" else " in favor of ${colors.blue replacement}";
    message = "${desc} is deprecated${suggestion}.";
  in warnWith handler "deprecated.${key}" message;

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

  warnEval = warnWith lib.warnIf;

  deprecatedOutput = category: name:
  warnEval "${category}.${name}" "The ${category} .#${name}";

  deprecatedApp = name: replacement: exe:
  deprecatedWith (appHandler exe) "app.${name}" "The app ${colors.blue ".#${name}"}" replacement;

  deprecatedLegacyApp = name: replacement: exe:
  deprecatedWith (legacyAppHandler exe) "app.${name}" "The app ${colors.blue ".#${name}"}" replacement;

in {
  inherit
  warnWith
  deprecatedWith
  deprecatedOutput
  deprecatedApp
  deprecatedLegacyApp
  ;
}
