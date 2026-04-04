{root = true;
  git = true;

  tests = {

    root1 = {
      source = ''
      describe 'root1 directory'
      output_exact 'root1-content'
      step cat content.txt
      '';

      git = false;
    };

    root2 = {
      source = ''
      describe 'root2 directory'
      output_exact 'root2-content'
      step cat content.txt
      '';
    };

  };
}

