{
  bounds = {
    root = {
      microlens = ">=0.4.0.1 && <0.5";
      semigroups = ">=0.19.1 && <0.21";
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
  lowerInit = {
    lower = {
      microlens = "0.4.0.1";
      semigroups = "0.19.1";
    };
  };
  resolving = false;
}
