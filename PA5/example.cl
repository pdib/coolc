
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class A {
  foo(): Int {{ 1337 /2 ;1337 + 2; }};
};

class B inherits A {
  b: Int <- 5;
};

class Main {
  bar(): String { "Hello world" };
  foo(): Bool { false };
  main(): Int { 0 };
};

