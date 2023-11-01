{config, lib, util}: let

  checkVersion = actual: bound: op: let
    check =
    if op == ">="
    then v: v >= 0
    else if op == ">"
    then v: v == 1
    else if op == "<="
    then v: v <= 0
    else if op == "<"
    then v: v < 0
    else if op == "=="
    then v: v == 0
    else if op == "!="
    then v: v != 0
    else throw "Invalid operator for version comparison in dependency condition: ${op}";
  in check (builtins.compareVersions actual bound);

in {

  os = {os}: {
    render = "os(${os})";
    satisfied = _: true;
  };

  impl = {version, op}: {
    render = "impl(ghc ${op} ${version})";
    satisfied = ghc: checkVersion ghc.ghc.version version op;
  };

}
