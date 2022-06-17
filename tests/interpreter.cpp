#include <catch2/catch.hpp>

#include "goldberg.hpp"

TEST_CASE("expressions can be parsed", "[parse]") {
  goldberg::Interpreter interpreter;

  REQUIRE(interpreter.parse("")->to_string() == "nil");
  REQUIRE(interpreter.parse("  \n  \n  \n")->to_string() == "nil");
  REQUIRE(interpreter.parse("\n; Comment test\n\n")->to_string() == "nil");

  REQUIRE(interpreter.parse("\"test\"")->to_string() == "\"test\"");
  REQUIRE(interpreter.parse("\"test\\\"quotes\\\"\"")->to_string() == "\"test\\\"quotes\\\"\"");
  REQUIRE(interpreter.parse("\"test\\nnewline\"")->to_string() == "\"test\\nnewline\"");
  REQUIRE(interpreter.parse("\"test \\\n    continuation\"")->to_string() == "\"test continuation\"");

  REQUIRE(interpreter.parse("0")->to_string() == "0");
  REQUIRE(interpreter.parse("+0.0")->to_string() == "0");
  REQUIRE(interpreter.parse("-1.5")->to_string() == "-1.5");
  REQUIRE(interpreter.parse(".25")->to_string() == "0.25");

  REQUIRE(interpreter.parse("()")->to_string() == "nil");
  REQUIRE(interpreter.parse("'()")->to_string() == "(quote nil)");
  REQUIRE(interpreter.parse("foobar")->to_string() == "foobar");
}

TEST_CASE("basic expressions can be evaluated", "[basic]") {
  goldberg::Interpreter interpreter;

  REQUIRE(interpreter.evaluate("nil")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("t")->to_string() == "t");

  REQUIRE(interpreter.evaluate("'foobar")->to_string() == "foobar");
  REQUIRE(interpreter.evaluate("'(foo 1.25 \"bar\" (baz))")->to_string() == "(foo 1.25 \"bar\" (baz))");

  REQUIRE(interpreter.evaluate("`(1 2 (+ 1 2))")->to_string() == "(1 2 (+ 1 2))");
  REQUIRE(interpreter.evaluate("`(1 2 ,(+ 1 2))")->to_string() == "(1 2 3)");
  REQUIRE(interpreter.evaluate("`(,@'(-1 0) 1 2 ,(+ 1 2) ,@(list 4 5 6))")->to_string() == "(-1 0 1 2 3 4 5 6)");

  REQUIRE(interpreter.evaluate("(progn)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(progn 1 2 (+ 1 2))")->to_string() == "3");

  REQUIRE(interpreter.evaluate("(eval 1)")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(eval '(+ 1 2))")->to_string() == "3");

  REQUIRE(interpreter.evaluate("(equal 1 1)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(equal 1 2)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(equal 1 \"1\")")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(equal '(1 2) '(1 2 3))")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(equal '(1 2 (3)) '(1 2 (3)))")->to_string() == "t");

  REQUIRE(interpreter.evaluate("(make-symbol \"hello\")")->to_string() == "hello");
}

TEST_CASE("conditional expressions can be evaluated", "[conditional]") {
  goldberg::Interpreter interpreter;

  REQUIRE(interpreter.evaluate("(if t 1)")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(if nil 1)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(if (if nil 1) 1 2)")->to_string() == "2");

  REQUIRE(interpreter.evaluate("(cond)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(cond ((= 1 2) 2) (t 3))")->to_string() == "3");
  REQUIRE(interpreter.evaluate("(cond ((= 1 1)) (t 3))")->to_string() == "t");

  REQUIRE(interpreter.evaluate("(and)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(and 1)")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(and nil 1)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(and 1 2)")->to_string() == "2");

  REQUIRE(interpreter.evaluate("(or)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(or nil)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(or nil nil 2)")->to_string() == "2");
  REQUIRE(interpreter.evaluate("(or nil 3 2)")->to_string() == "3");

  REQUIRE(interpreter.evaluate("(not nil)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(not (null nil))")->to_string() == "nil");
}

TEST_CASE("numeric expressions can be evaluated", "[numeric]") {
  goldberg::Interpreter interpreter;

  REQUIRE(interpreter.evaluate("(+)")->to_string() == "0");
  REQUIRE(interpreter.evaluate("(+ 1.0)")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(+ (+ 0.5 0.5) 1.5)")->to_string() == "2.5");

  REQUIRE(interpreter.evaluate("(- 1.0)")->to_string() == "-1");
  REQUIRE(interpreter.evaluate("(- 1.0 2)")->to_string() == "-1");
  REQUIRE(interpreter.evaluate("(- (+ 0.5 0.5) 1.5)")->to_string() == "-0.5");

  REQUIRE(interpreter.evaluate("(1+ 1)")->to_string() == "2");
  REQUIRE(interpreter.evaluate("(1- 1)")->to_string() == "0");

  REQUIRE(interpreter.evaluate("(*)")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(* 2.0)")->to_string() == "2");
  REQUIRE(interpreter.evaluate("(* (+ 1.5 1.5) 2)")->to_string() == "6");

  REQUIRE(interpreter.evaluate("(/ 2.0)")->to_string() == "0.5");
  REQUIRE(interpreter.evaluate("(/ (+ 1.5 1.5) 2)")->to_string() == "1.5");
  REQUIRE(interpreter.evaluate("(/ 1 2 2)")->to_string() == "0.25");

  REQUIRE(interpreter.evaluate("(rem 13 4)")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(rem -1 5)")->to_string() == "-1");
  REQUIRE(interpreter.evaluate("(rem -13.4 1)")->to_string() == "-0.4");

  REQUIRE(interpreter.evaluate("(mod 13 4)")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(mod -1 5)")->to_string() == "4");
  REQUIRE(interpreter.evaluate("(mod -13.4 1)")->to_string() == "0.6");

  REQUIRE(interpreter.evaluate("(sin 0)")->to_string() == "0");
  REQUIRE(interpreter.evaluate("(sin (/ pi 2))")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(asin 0)")->to_string() == "0");
  REQUIRE(interpreter.evaluate("(cos 0)")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(cos pi)")->to_string() == "-1");
  REQUIRE(interpreter.evaluate("(acos 1)")->to_string() == "0");
  REQUIRE(interpreter.evaluate("(tan 0)")->to_string() == "0");
  REQUIRE(interpreter.evaluate("(atan 0)")->to_string() == "0");
  REQUIRE(interpreter.evaluate("(atan 0 0)")->to_string() == "0");

  REQUIRE(interpreter.evaluate("(floor 0.75)")->to_string() == "0");
  REQUIRE(interpreter.evaluate("(ceiling 0.75)")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(truncate -0.75)")->to_string() == "-0");
  REQUIRE(interpreter.evaluate("(round 0.75)")->to_string() == "1");

  REQUIRE(interpreter.evaluate("(abs -5)")->to_string() == "5");
  REQUIRE(interpreter.evaluate("(sqrt 9)")->to_string() == "3");
  REQUIRE(interpreter.evaluate("(exp 0)")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(expt 3 2)")->to_string() == "9");
  REQUIRE(interpreter.evaluate("(log (exp 1))")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(log 1024 2)")->to_string() == "10");

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

  REQUIRE(interpreter.evaluate("(min 1)")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(min 2 1 3)")->to_string() == "1");

  REQUIRE(interpreter.evaluate("(max 1)")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(max 2 1 3)")->to_string() == "3");
}

TEST_CASE("list expressions can be evaluated", "[list]") {
  goldberg::Interpreter interpreter;

  REQUIRE(interpreter.evaluate("(cons 0 nil)")->to_string() == "(0)");
  REQUIRE(interpreter.evaluate("(cons 0 1)")->to_string() == "(0 . 1)");
  REQUIRE(interpreter.evaluate("(cons 0 '(1 2 3))")->to_string() == "(0 1 2 3)");

  REQUIRE(interpreter.evaluate("(list)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(list 1 (+ 1 1) 3)")->to_string() == "(1 2 3)");

  REQUIRE(interpreter.evaluate("(car '(1 2 3))")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(car nil)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(first '(2 . 3)")->to_string() == "2");

  REQUIRE(interpreter.evaluate("(cdr '(1 2 3))")->to_string() == "(2 3)");
  REQUIRE(interpreter.evaluate("(cdr nil)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(rest '(2 . 3)")->to_string() == "3");

  REQUIRE(interpreter.evaluate("(nth 0 '(1 2 3))")->to_string() == "1");
  REQUIRE(interpreter.evaluate("(nth 1 '(1 2 3))")->to_string() == "2");
  REQUIRE(interpreter.evaluate("(nth 3 '(1 2 3))")->to_string() == "nil");

  REQUIRE(interpreter.evaluate("(length nil)")->to_string() == "0");
  REQUIRE(interpreter.evaluate("(length '(1 2 3))")->to_string() == "3");

  REQUIRE(interpreter.evaluate("(append)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(append nil nil nil)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(append '(1) nil '(2 3))")->to_string() == "(1 2 3)");

  REQUIRE(interpreter.evaluate("(last nil)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(last '(1 2 3))")->to_string() == "(3)");
  REQUIRE(interpreter.evaluate("(last '(1 2 3) 2)")->to_string() == "(2 3)");

  REQUIRE(interpreter.evaluate("(reverse nil)")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("(reverse '(1 2 3))")->to_string() == "(3 2 1)");

  REQUIRE(interpreter.evaluate("(concatenate 'list '(1 2 3) '(4 5 6))")->to_string() == "(1 2 3 4 5 6)");
}

TEST_CASE("string expressions can be evaluated", "[string]") {
  goldberg::Interpreter interpreter;

  REQUIRE(interpreter.evaluate("(write-to-string 3.14)")->to_string() == "\"3.14\"");

  REQUIRE(interpreter.evaluate("(concatenate 'string \"hello\" \"goodbye\")")->to_string() == "\"hellogoodbye\"");
}

TEST_CASE("lambda expressions can be evaluated", "[lambda]") {
  goldberg::Interpreter interpreter;

  REQUIRE(interpreter.evaluate("((lambda ()))")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("((lambda () 1 2 3))")->to_string() == "3");
  REQUIRE(interpreter.evaluate("((lambda (a) (+ a 1)) (* 3 3))")->to_string() == "10");

  REQUIRE(interpreter.evaluate("((lambda (&optional b) b))")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("((lambda (&optional b) b) 5)")->to_string() == "5");
  REQUIRE(interpreter.evaluate("((lambda (&optional (b 5)) b))")->to_string() == "5");
  REQUIRE(interpreter.evaluate("((lambda (&optional (b 5 b_set)) b_set))")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("((lambda (&optional (b 5 b_set)) b_set) 5)")->to_string() == "t");

  REQUIRE(interpreter.evaluate("((lambda (&optional b &rest rest) rest))")->to_string() == "nil");
  REQUIRE(interpreter.evaluate("((lambda (&optional b &rest rest) rest) 3 2 1)")->to_string() == "(2 1)");

  REQUIRE(interpreter.evaluate("((lambda (a &key b c d) (list b c)) 1 :c 2 :b 1)")->to_string() == "(1 2)");

  REQUIRE(interpreter.evaluate("((lambda (a) (setq a 2) (+ a 1)) (* 3 3))")->to_string() == "3");
}

TEST_CASE("interpreter environment can be manipulated", "[environment]") {
  goldberg::Interpreter interpreter;

  REQUIRE(interpreter.evaluate("(defparameter a (+ 1 1))")->to_string() == "a");
  REQUIRE(interpreter.evaluate("(defparameter b (+ a 1))")->to_string() == "b");
  REQUIRE(interpreter.evaluate("(+ a b)")->to_string() == "5");

  REQUIRE(interpreter.evaluate("(defvar a 100)")->to_string() == "a");
  REQUIRE(interpreter.evaluate("a")->to_string() == "2");
  REQUIRE(interpreter.evaluate("(defvar c 100)")->to_string() == "c");
  REQUIRE(interpreter.evaluate("c")->to_string() == "100");

  REQUIRE(interpreter.evaluate("(defconstant d 10)")->to_string() == "d");
  REQUIRE(interpreter.evaluate("(+ d 10)")->to_string() == "20");

  REQUIRE(interpreter.evaluate("(defun do_something () (+ a c d))")->to_string() == "do_something");
  REQUIRE(interpreter.evaluate("(do_something)")->to_string() == "112");

  REQUIRE(interpreter.evaluate("(defmacro test_macro () `(+ a c ,(+ d 10)))")->to_string() == "test_macro");
  REQUIRE(interpreter.evaluate("(test_macro)")->to_string() == "122");
  REQUIRE(interpreter.evaluate("((lambda (a) (test_macro)) 100)")->to_string() == "220");
}

TEST_CASE("built-in macros can be called", "[macros]") {
  goldberg::Interpreter interpreter;

  REQUIRE(interpreter.evaluate("(let () t)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(let ((a 1)) (+ a 1))")->to_string() == "2");

  REQUIRE(interpreter.evaluate("(second '(1 2 3 4 5 6 7 8 9 10))")->to_string() == "2");
  REQUIRE(interpreter.evaluate("(third '(1 2 3 4 5 6 7 8 9 10))")->to_string() == "3");
  REQUIRE(interpreter.evaluate("(fourth '(1 2 3 4 5 6 7 8 9 10))")->to_string() == "4");
  REQUIRE(interpreter.evaluate("(fifth '(1 2 3 4 5 6 7 8 9 10))")->to_string() == "5");
  REQUIRE(interpreter.evaluate("(sixth '(1 2 3 4 5 6 7 8 9 10))")->to_string() == "6");
  REQUIRE(interpreter.evaluate("(seventh '(1 2 3 4 5 6 7 8 9 10))")->to_string() == "7");
  REQUIRE(interpreter.evaluate("(eighth '(1 2 3 4 5 6 7 8 9 10))")->to_string() == "8");
  REQUIRE(interpreter.evaluate("(ninth '(1 2 3 4 5 6 7 8 9 10))")->to_string() == "9");
  REQUIRE(interpreter.evaluate("(tenth '(1 2 3 4 5 6 7 8 9 10))")->to_string() == "10");

  REQUIRE(interpreter.evaluate("(cadr '(1 2 3 4 5 6 7 8 9 10))")->to_string() == "2");
  REQUIRE(interpreter.evaluate("(caddr '(1 2 3 4 5 6 7 8 9 10))")->to_string() == "3");
  REQUIRE(interpreter.evaluate("(cadddr '(1 2 3 4 5 6 7 8 9 10))")->to_string() == "4");
  REQUIRE(interpreter.evaluate("(cddddr '(1 2 3 4 5 6 7 8 9 10))")->to_string() == "(5 6 7 8 9 10)");

  REQUIRE(interpreter.evaluate("(defvar *foo* 1)")->to_string() == "*foo*");
  REQUIRE(interpreter.evaluate("(incf *foo*)")->to_string() == "2");
  REQUIRE(interpreter.evaluate("(incf *foo*)")->to_string() == "3");
  REQUIRE(interpreter.evaluate("(decf *foo*)")->to_string() == "2");

  REQUIRE(interpreter.evaluate("(gensym)")->to_string() == "G1");
  REQUIRE(interpreter.evaluate("(gensym \"P\")")->to_string() == "P2");
}

TEST_CASE("random primitives can be called", "[random]") {
  goldberg::Interpreter interpreter;

  REQUIRE(interpreter.evaluate("(<= 0 (random 1) 1)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(<= 0 (random 1 (make-random-state)) 1)")->to_string() == "t");
  REQUIRE(interpreter.evaluate("(<= 0 (random 1 (make-random-state 12345)) 1)")->to_string() == "t");
}
