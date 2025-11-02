{
  source = ''
    check_musl()
    {
      describe "Build musl binary for $1"
      step_build $1

      describe "Run musl binary for $1"
      output_exact 'string'
      step result/bin/exe

      describe "Check that the executable for $1 is linked against musl"
      output_match 'libc.so =>.*musl'
      error_ignore
      step ldd result/bin/exe
    }

    check_musl "root.musl"

    check_musl "root.cross.musl64"

    check_musl "root.cross.musl64.static"

    check_musl "ghc910.root.cross.musl64.static"

    check_musl "env.cross.static-override"

    check_static()
    {
      describe "Build static binary for $1"
      step_build $1

      describe "Run static binary for $1"
      output_exact 'string'
      step result/bin/exe

      describe "Check that the executable for $1 is linked statically"
      output_match 'statically linked'
      error_ignore
      step file result/bin/exe
    }

    check_static "root.static"

    check_static "root.executables.exe.static"

    check_static "dev.root.executables.exe.static"

    check_static "env.dev.root.executables.exe.static"

    check_appimage()
    {
      describe "Build AppImage for $1"
      step_build $1

      describe "Run AppImage for $1"
      output_exact 'string'
      step ./result
    }

    check_appimage 'appimage'

    check_appimage 'root.appimage'

    check_appimage 'alias.appimage'

    check_appimage 'env.ghc910.appimage'

    check_appimage 'env.ghc910.root.appimage'

    check_appimage 'env.ghc910.exe.appimage'
  '';
}
