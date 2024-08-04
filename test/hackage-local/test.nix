{testlib, ...}: {
  git = true;
} // testlib.hackage.withServer ''
local target="Uploading
/nix/store/hash-containers-1.0.0.0-sdist/containers-1.0.0.0.tar.gz...
Package successfully published. You can now view it at
'http://localhost:$port/package/containers-1.0.0.0'.
Uploading documentation
/nix/store/hash-containers-1.0.0.0-haddock/containers-1.0.0.0-docs.tar.gz...
Package documentation successfully published. You can now view it at
'http://localhost:$port/package/containers-1.0.0.0'."

preproc_output 'sub_store_hash | drop_end 2'
output_exact $target
step nix run .#release -- -v 1.0.0.0
''
