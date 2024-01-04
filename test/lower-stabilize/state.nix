{
  bounds = {
    root = {
      base = {
        lower = "4.15.1.0";
        upper = null;
      };
      microlens = {
        lower = "0.4.0.1";
        upper = "0.5";
      };
      semigroups = {
        lower = "0.19.1";
        upper = "0.21";
      };
    };
  };
  versions = {
    lower = {
      base = "4.15.1.0";
      microlens = "0.4.0.1";
      semigroups = "0.19.1";
    };
  };
  initial = {
    lower = {
      base = "4.15.1.0";
      microlens = "0.4.13.1";
      semigroups = "0.20";
    };
  };
  overrides = {
    lower = {
      microlens = {
        version = "0.4.0.1";
        hash = "0fib31m9fbj9a3kn1vqnq52glipc2fzxsjg6kj62ycx09wbdrrrb";
      };
      semigroups = {
        version = "0.19.1";
        hash = "0ng4j78di5p2yqx3m4bnfcz9cplp5px7d95374w2qxi20spjabpi";
      };
    };
  };
  resolving = false;
}
