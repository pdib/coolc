
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class A {
  a: Int <- 10;
  c: Int <- let x: Int <- 3 in a + x;
  bar(a: Int): Int {{ 1337 /2 ;1337 + 2; a;}};
  foo(a: Int): Int {{ 1337 /2 ;1337 + 2; a;}};
};

class B inherits A {
  b: Int <- a - c;
  bar(b: Int): Int {{ b; }};
};

class Main {
  bar(): String { "Hello world" };
  foo(): Bool { false };
  main(): Int { (new B).bar(55) };
};

