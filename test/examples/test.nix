{

  path = p: [p.rsync];

  source = ''
  test_example()
  {
    source=$1
    name=''${source:t}
    target="./$name"
    step rsync -rlt --chmod=u+w $source/ $target/
    pushd $target
    step sed -i -e "/inputs\.hix/ s#\".*\"#\"path:$hix_dir\"#" flake.nix
    step_nix --quiet flake update
    source test.zsh
    popd
  }

  export hix_dir
  for dir in examples/*
  do
    test_example $dir
  done
  '';

}
