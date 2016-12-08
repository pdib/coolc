
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class A {
  bar(a: Int): Int {{ 1337 /2 ;1337 + 2; a;}};
  foo(a: Int): Int {{ 1337 /2 ;1337 + 2; a;}};
};

class B inherits A {
  b: Int <- 5;
  bar(a: Int): Int {{ 0; }};
};

class Main {
  bar(): String { "Hello world" };
  foo(): Bool { false };
  main(): Int { (new B)@A.bar(55) };
};

