#include <catch2/catch.hpp>

#include "goldberg.hpp"

TEST_CASE("expressions can be parsed", "[parse]") {
  goldberg::Interpreter interpreter;

  REQUIRE(interpreter.evaluate("")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("  \n  \n  \n")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("\n; Comment test\n\n")->to_string() == "nil");

  REQUIRE(interpreter.evaluate("\"test\"")->to_string() == "\"test\"");
  REQUIRE(interpreter.evaluate("\"test\\\"quotes\\\"\"")->to_string() == "\"test\\\"quotes\\\"\"");
  REQUIRE(interpreter.evaluate("\"test\\nnewline\"")->to_string() == "\"test\\nnewline\"");
  REQUIRE(interpreter.evaluate("\"test \\\n    continuation\"")->to_string() == "\"test continuation\"");

  REQUIRE(interpreter.evaluate("0")->to_string() == "0");
  REQUIRE(interpreter.evaluate("+0.0")->to_string() == "0");
  REQUIRE(interpreter.evaluate("-1.5")->to_string() == "-1.5");
  REQUIRE(interpreter.evaluate(".25")->to_string() == "0.25");

  REQUIRE(interpreter.evaluate("()")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("'()")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("nil")->to_string() == "nil");

  REQUIRE(interpreter.evaluate("t")->to_string() == "t");

  REQUIRE(interpreter.evaluate("'foobar")->to_string() == "foobar");
  REQUIRE(interpreter.evaluate("'(foo 1.25 \"bar\" (baz))")->to_string() == "(foo 1.25 \"bar\" (baz))");
}

TEST_CASE("expressions can be evaluated", "[evaluate]") {
  goldberg::Interpreter interpreter;

  REQUIRE(interpreter.evaluate("(if t 1)")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(if nil 1)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(if (if nil 1) 1 2)")->to_string() == "2");

  REQUIRE(interpreter.evaluate("(and)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(and 1)")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(and nil 1)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(and 1 2)")->to_string() == "2");

  REQUIRE(interpreter.evaluate("(or)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(or nil)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(or nil nil 2)")->to_string() == "2");
  REQUIRE(interpreter.evaluate("(or nil 3 2)")->to_string() == "3");

  REQUIRE(interpreter.evaluate("(+)")->to_string() == "0");
  REQUIRE(interpreter.evaluate("(+ 1.0)")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(+ (+ 0.5 0.5) 1.5)")->to_string() == "2.5");

  REQUIRE(interpreter.evaluate("(- 1.0)")->to_string() == "-1");
  REQUIRE(interpreter.evaluate("(- 1.0 2)")->to_string() == "-1");
  REQUIRE(interpreter.evaluate("(- (+ 0.5 0.5) 1.5)")->to_string() == "-0.5");

  REQUIRE(interpreter.evaluate("(*)")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(* 2.0)")->to_string() == "2");
  REQUIRE(interpreter.evaluate("(* (+ 1.5 1.5) 2)")->to_string() == "6");

  REQUIRE(interpreter.evaluate("(/ 2.0)")->to_string() == "0.5");
  REQUIRE(interpreter.evaluate("(/ (+ 1.5 1.5) 2)")->to_string() == "1.5");
  REQUIRE(interpreter.evaluate("(/ 1 2 2)")->to_string() == "0.25");

  REQUIRE(interpreter.evaluate("(not nil)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(not (null nil))")->to_string() == "nil");

  REQUIRE(interpreter.evaluate("(= 0)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(= 0 0)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(= 0 1)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(= 0 0 0)")->to_string() == "t");

  REQUIRE(interpreter.evaluate("(< 0)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(< 0 0)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(< 0 1)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(< 1 2 3)")->to_string() == "t");

  REQUIRE(interpreter.evaluate("(> 0)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(> 0 0)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(> 1 0)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(> 3 2 1)")->to_string() == "t");

  REQUIRE(interpreter.evaluate("(<= 0)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(<= 0 0)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(<= 1 0)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(<= 1 2 2)")->to_string() == "t");

  REQUIRE(interpreter.evaluate("(>= 0)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(>= 0 0)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(>= 1 2)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(>= 3 2 2)")->to_string() == "t");

  REQUIRE(interpreter.evaluate("(/= 0)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(/= 0 1)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(/= 0 1 0)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(/= 0 1 2)")->to_string() == "t");
}
