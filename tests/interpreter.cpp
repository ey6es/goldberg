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
