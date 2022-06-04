#include <catch2/catch.hpp>

#include "goldberg.hpp"

TEST_CASE("expressions can be parsed", "[parser]") {
  goldberg::Interpreter interpreter;

  REQUIRE(interpreter.evaluate("")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("  \n  \n  \n")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("\n; Comment test\n\n")->to_string() == "nil");

  REQUIRE(interpreter.evaluate("\"test\"")->to_string() == "\"test\"");
  REQUIRE(interpreter.evaluate("0")->to_string() == "0");
  REQUIRE(interpreter.evaluate("+0.0")->to_string() == "0");
  REQUIRE(interpreter.evaluate("-1.5")->to_string() == "-1.5");
  REQUIRE(interpreter.evaluate(".25")->to_string() == "0.25");

  REQUIRE(interpreter.evaluate("()")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("'()")->to_string() == "(quote nil)");
}
