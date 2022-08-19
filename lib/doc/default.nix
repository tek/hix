{ pkgs, inputs }:
let

  nmd = import inputs.nmd { inherit (pkgs) lib; inherit pkgs; };

  modulesDocs = nmd.buildModulesDocs {
    modules = import ../../modules/all-modules.nix { inherit inputs; projectModules = []; };
    moduleRootPaths = [../../modules];
    mkModuleUrl = path: "https://git.tryp.io/tek/hix/src/branch/main/${path}";
    channelName = "hix";
    docBook.id = "hix-options";
  };

  docs = nmd.buildDocBookDocs {
    pathName = "hix-docs";
    modulesDocs = [modulesDocs];
    documentsDirectory = ./.;
    chunkToc = ''
      <toc>
        <d:tocentry xmlns:d="http://docbook.org/ns/docbook" linkend="book-hix-manual"><?dbhtml filename="index.html"?>
          <d:tocentry linkend="ch-options"><?dbhtml filename="options.html"?></d:tocentry>
        </d:tocentry>
      </toc>
    '';
  };

in {
  inherit modulesDocs docs;
}
