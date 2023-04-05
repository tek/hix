{
  pkgs,
  util,
  header,
  chapters,
}:
with pkgs.lib;

let
  nixpkgsDoc = pkgs.path + "/doc";
  manpageUrls = nixpkgsDoc + "/manpage-urls.json";

  renderOptions = {name, options, header, extra}: ''
  ### ${header} options {#options-${name}}

  ${extra}

  ```{=include=} options
  id-prefix: opt-${name}-
  list-id: configuration-variable-list
  source: ${options}/share/doc/nixos/options.json
  ```

  '';

  render = {type, content}:
  if type == "text"
  then content
  else renderOptions content;

  renderChapter = {tag, heading, fragments}: pkgs.writeText "chapter-${tag}.md" ''
  # ${heading} {#${tag}}
  ${util.unlines (map render fragments)}
  '';

  renderChapters = util.unlines (map renderChapter chapters);

  manualMd = pkgs.writeText "manual.md" ''
  ${header}

  ```{=include=} chapters
  ${renderChapters}
  ```
  '';

  renderManual = pkgs.runCommand "nixos-manual-html" {
    nativeBuildInputs = [pkgs.nixos-render-docs];
    meta.description = "The Hix manual in HTML format";
    allowedReferences = ["out"];
  }
  ''
    dst=$out/share/doc/hix
    mkdir -p $dst
    cp ${nixpkgsDoc + "/style.css"} $dst/style.css
    cp ${nixpkgsDoc + "/overrides.css"} $dst/overrides.css
    cp -r ${pkgs.documentation-highlighter} $dst/highlightjs
    nixos-render-docs -j $NIX_BUILD_CORES manual html \
      --manpage-urls ${manpageUrls} \
      --revision default \
      --stylesheet style.css \
      --stylesheet overrides.css \
      --stylesheet highlightjs/mono-blue.css \
      --script ./highlightjs/highlight.pack.js \
      --script ./highlightjs/loader.js \
      --toc-depth 2 \
      --chunk-toc-depth 2 \
      ${manualMd} \
      $dst/index.html
  '';


in {
  inherit renderManual;
  inherit (optionsDoc) optionsJSON;
}
