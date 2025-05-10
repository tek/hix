{util, ...}: {
  systems = ["x86_64-linux"];
  outputs.legacyPackages.b = util.fromMaybeNull 3 3;
  name = "b";
}
