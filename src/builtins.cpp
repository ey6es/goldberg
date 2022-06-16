#include <cmath>
#include <functional>
#include <unordered_set>
#include <utility>

#include "goldberg.hpp"

namespace goldberg {

namespace {

inline void define (bindings& ctx, const std::string& name, const std::shared_ptr<Value>& value) {
  ctx[Interpreter::static_symbol_value(name)] = value;
}

template<typename Expand>
inline void define_expander (bindings& ctx, const std::string& name, const Expand& expand) {
  define(ctx, name, std::make_shared<Expander<Expand>>(name, expand));
}

template<typename Invoke>
inline void define_operator (bindings& ctx, const std::string& name, const Invoke& invoke) {
  define(ctx, name, std::make_shared<Operator<Invoke>>(name, invoke));
}

template<typename Expand, typename Invoke>
inline void define_expand_operator (bindings& ctx, const std::string& name, const Expand& expand, const Invoke& invoke) {
  define(ctx, name, std::make_shared<ExpandOperator<Expand, Invoke>>(name, expand, invoke));
}

template<typename Invoke>
inline void define_native_function (bindings& ctx, const std::string& name, const Invoke& invoke) {
  define(ctx, name, std::make_shared<NativeFunction<Invoke>>(name, invoke));
}

inline std::shared_ptr<Value> require_1 (const std::shared_ptr<Value>& args) {
  auto pair = args->require_pair(args);
  pair->right()->require_nil();
  return pair->left();
}

inline double require_1_number (const std::shared_ptr<Value>& args) {
  return require_1(args)->require_number(*args->loc());
}

inline std::pair<std::shared_ptr<Value>, std::shared_ptr<Value>> require_2 (const std::shared_ptr<Value>& args) {
  auto first_pair = args->require_pair(args);
  auto second_pair = first_pair->right()->require_pair(first_pair->right());
  second_pair->right()->require_nil();
  return std::make_pair(first_pair->left(), second_pair->left());
}

inline std::pair<double, double> require_2_numbers (const std::shared_ptr<Value>& args) {
  auto first_pair = args->require_pair(args);
  auto second_pair = first_pair->right()->require_pair(first_pair->right());
  second_pair->right()->require_nil();
  return std::make_pair(
    first_pair->left()->require_number(*first_pair->loc()),
    second_pair->left()->require_number(*second_pair->loc()));
}

template<typename T>
inline std::shared_ptr<Value> fold_numbers (const std::shared_ptr<Value>& args, double initial, const T& combine) {
  double value = initial;
  auto next = args;
  while (*next) {
    auto next_pair = next->require_pair(next);
    value = combine(value, next_pair->left()->require_number(*next_pair->loc()));
    next = next_pair->right();
  }
  return std::make_shared<Number>(value);
}

template<typename T>
inline std::shared_ptr<Value> reduce_numbers (const std::shared_ptr<Value>& args, const T& combine) {
  auto first_pair = args->require_pair(args);
  return fold_numbers(first_pair->right(), first_pair->left()->require_number(*first_pair->loc()), combine);
}

template<typename T>
inline std::shared_ptr<Value> compare_numbers (const std::shared_ptr<Value>& args, const T& compare) {
  auto first_pair = args->require_pair(args);
  auto value = first_pair->left()->require_number(*first_pair->loc());
  auto next = first_pair->right();
  while (*next) {
    auto next_pair = next->require_pair(next);
    auto next_value = next_pair->left()->require_number(*next_pair->loc());
    if (!compare(value, next_value)) return Interpreter::nil();
    value = next_value;
    next = next_pair->right();
  }
  return Interpreter::t();
}

std::pair<std::shared_ptr<Value>, int> last (const std::shared_ptr<Value>& list, int count) {
  if (!*list) return std::make_pair(Interpreter::nil(), 0);
  auto first_pair = list->require_pair(list);
  auto result = last(first_pair->right(), count);
  return result.second < count
    ? std::make_pair(std::make_shared<Pair>(first_pair->left(), result.first), result.second + 1)
    : result;
}

}

bool Interpreter::populate_static_bindings () {
  auto& ctx = static_bindings_;

  define(ctx, "nil", nil_);
  define(ctx, "t", t_);
  define(ctx, "pi", std::make_shared<Number>(M_PI));

  define_expand_operator(
    ctx, "quote",
    [](Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) {
      return std::make_shared<Pair>(self, pair.right(), pair.loc());
    },
    [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
      return require_1(args);
    });

  define_expand_operator(
    ctx, "backquote",
    [](Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) {
      return std::make_shared<Pair>(self, pair.right()->compile_commas(interpreter, pair.right()), pair.loc());
    },
    [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
      auto arg = require_1(args);
      return arg->evaluate_commas(interpreter, arg);
    });

  define_expand_operator(
    ctx, "defconstant",
    [](Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) {
      auto symbol_pair = pair.right()->require_pair(pair.right());
      auto symbol = symbol_pair->left();
      auto symbol_value = symbol->require_symbol(*symbol_pair->loc());
      auto value_pair = symbol_pair->right()->require_pair(symbol_pair->right());
      auto new_value = value_pair->left()->compile(interpreter, value_pair->left());
      value_pair->right()->require_nil();

      return std::make_shared<Pair>(
        self,
        std::make_shared<Pair>(symbol, std::make_shared<Pair>(new_value, Interpreter::nil())),
        pair.loc());
    },
    [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
      auto symbol_pair = args->require_pair(args);
      auto symbol = symbol_pair->left();
      auto symbol_value = symbol->require_symbol(*symbol_pair->loc());
      auto value_pair = symbol_pair->right()->require_pair(symbol_pair->right());
      auto new_value = value_pair->left()->evaluate(interpreter, value_pair->left());
      value_pair->right()->require_nil();

      interpreter.top_level_bindings()[symbol_value] = new_value;

      return symbol;
    });

  define_expand_operator(
    ctx, "defparameter",
    [](Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) {
      auto symbol_pair = pair.right()->require_pair(pair.right());
      auto symbol = symbol_pair->left();
      auto symbol_value = symbol->require_symbol(*symbol_pair->loc());
      auto value_pair = symbol_pair->right()->require_pair(symbol_pair->right());
      auto new_value = value_pair->left()->compile(interpreter, value_pair->left());
      value_pair->right()->require_nil();

      interpreter.top_level_bindings()[symbol_value] = std::make_shared<DynamicVariable>(symbol_value);

      return std::make_shared<Pair>(
        self,
        std::make_shared<Pair>(symbol, std::make_shared<Pair>(new_value, Interpreter::nil())),
        pair.loc());
    },
    [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
      auto symbol_pair = args->require_pair(args);
      auto symbol = symbol_pair->left();
      auto symbol_value = symbol->require_symbol(*symbol_pair->loc());
      auto value_pair = symbol_pair->right()->require_pair(symbol_pair->right());
      auto new_value = value_pair->left()->evaluate(interpreter, value_pair->left());
      value_pair->right()->require_nil();

      interpreter.top_level_context()->define(symbol_value, new_value);

      return symbol;
    });

  define_expand_operator(
    ctx, "defvar",
    [](Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) {
      auto symbol_pair = pair.right()->require_pair(pair.right());
      auto symbol = symbol_pair->left();
      auto symbol_value = symbol->require_symbol(*symbol_pair->loc());

      auto value_pair = symbol_pair->right()->as_pair(symbol_pair->right());
      auto new_value_pair = Interpreter::nil();
      if (value_pair) {
        auto new_value = value_pair->left()->compile(interpreter, value_pair->left());
        value_pair->right()->require_nil();
        new_value_pair = std::make_shared<Pair>(new_value, Interpreter::nil());

      } else symbol_pair->right()->require_nil();

      interpreter.top_level_bindings()[symbol_value] = std::make_shared<DynamicVariable>(symbol_value);

      return std::make_shared<Pair>(self, std::make_shared<Pair>(symbol, new_value_pair), pair.loc());
    },
    [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
      auto symbol_pair = args->require_pair(args);
      auto symbol = symbol_pair->left();
      auto symbol_value = symbol->require_symbol(*symbol_pair->loc());

      auto value_pair = symbol_pair->right()->as_pair(symbol_pair->right());
      auto new_value = Interpreter::nil();
      if (value_pair) {
        new_value = value_pair->left()->evaluate(interpreter, value_pair->left());
        value_pair->right()->require_nil();
      }

      if (!interpreter.top_level_context()->is_defined(symbol_value)) {
        interpreter.top_level_context()->define(symbol_value, new_value);
      }

      return symbol;
    });

  define_expand_operator(
    ctx, "defun",
    [](Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) {
      auto symbol_pair = pair.right()->require_pair(pair.right());
      auto symbol = symbol_pair->left();
      auto symbol_value = symbol->require_symbol(*symbol_pair->loc());
      auto new_value = std::make_shared<LambdaDefinition>(*symbol_value, interpreter, symbol_pair->right());

      return std::make_shared<Pair>(
        self,
        std::make_shared<Pair>(symbol, std::make_shared<Pair>(new_value, Interpreter::nil())),
        pair.loc());
    },
    [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
      auto symbol_pair = args->require_pair(args);
      auto symbol = symbol_pair->left();
      auto symbol_value = symbol->require_symbol(*symbol_pair->loc());
      auto value_pair = symbol_pair->right()->require_pair(symbol_pair->right());
      auto new_value = value_pair->left()->evaluate(interpreter, value_pair->left());
      value_pair->right()->require_nil();

      interpreter.top_level_bindings()[symbol_value] = new_value;

      return symbol;
    });

  define_expand_operator(
    ctx, "defmacro",
    [](Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) {
      auto symbol_pair = pair.right()->require_pair(pair.right());
      auto symbol = symbol_pair->left();
      auto symbol_value = symbol->require_symbol(*symbol_pair->loc());
      auto new_value = std::make_shared<LambdaDefinition>(*symbol_value, interpreter, symbol_pair->right());

      return std::make_shared<Pair>(
        self,
        std::make_shared<Pair>(symbol, std::make_shared<Pair>(new_value, Interpreter::nil())),
        pair.loc());
    },
    [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
      auto symbol_pair = args->require_pair(args);
      auto symbol = symbol_pair->left();
      auto symbol_value = symbol->require_symbol(*symbol_pair->loc());
      auto value_pair = symbol_pair->right()->require_pair(symbol_pair->right());
      auto new_value = value_pair->left()->evaluate(interpreter, value_pair->left());
      value_pair->right()->require_nil();

      interpreter.top_level_bindings()[symbol_value] = std::make_shared<Macro>(new_value);

      return symbol;
    });

  define_expander(
    ctx, "lambda",
    [](Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) {
      return std::make_shared<LambdaDefinition>("lambda", interpreter, pair.right());
    });

  define_operator(ctx, "if", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto cond_pair = args->require_pair(args);
    auto true_pair = cond_pair->right()->require_pair(cond_pair->right());
    auto false_expr = true_pair->right();
    if (!false_expr->is_nil()) {
      auto false_pair = false_expr->require_pair(false_expr);
      false_expr = false_pair->left();
      false_pair->right()->require_nil();
    }
    return *cond_pair->left()->evaluate(interpreter, cond_pair->left())
      ? true_pair->left()->evaluate(interpreter, true_pair->left())
      : false_expr->evaluate(interpreter, false_expr);
  });

  define_operator(ctx, "and", [=](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto value = t_;
    auto next = args;
    while (*next) {
      auto next_pair = next->require_pair(next);
      value = next_pair->left()->evaluate(interpreter, next_pair->left());
      if (!*value) return nil_;
      next = next_pair->right();
    }
    return value;
  });

  define_operator(ctx, "or", [=](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto next = args;
    while (*next) {
      auto next_pair = next->require_pair(next);
      auto value = next_pair->left()->evaluate(interpreter, next_pair->left());
      if (*value) return value;
      next = next_pair->right();
    }
    return nil_;
  });

  define_operator(ctx, "setq", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto next = args;
    auto last_result = Interpreter::nil();
    while (*next) {
      auto next_pair = next->require_pair(next);
      auto variable = next_pair->left();
      next = next_pair->right();
      next_pair = next->require_pair(next);
      last_result = next_pair->left()->evaluate(interpreter, next_pair->left());
      variable->set_value(interpreter, last_result, *args->loc());
      next = next_pair->right();
    }
    return last_result;
  });

  define_native_function(ctx, "eval", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto arg = require_1(args);
    auto compiled = arg->compile(interpreter, arg);
    return compiled->evaluate(interpreter, compiled);
  });

  define_native_function(ctx, "load", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return interpreter.load(require_1(args)->require_string(*args->loc()));
  });
  define_native_function(ctx, "require", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return interpreter.require(require_1(args)->require_string(*args->loc()));
  });

  define_native_function(ctx, "eq", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto pair = require_2(args);
    return pair.first == pair.second ? t_ : nil_;
  });
  define_native_function(ctx, "equal", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto pair = require_2(args);
    return pair.first->equals(pair.second) ? t_ : nil_;
  });

  define_native_function(ctx, "+", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return fold_numbers(args, 0.0, std::plus<double>());
  });

  define_native_function(ctx, "-", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto first_pair = args->require_pair(args);
    double initial = first_pair->left()->require_number(*first_pair->loc());
    auto rest = first_pair->right();
    return *rest
      ? fold_numbers(rest, initial, std::minus<double>())
      : std::make_shared<Number>(-initial);
  });

  define_native_function(ctx, "*", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return fold_numbers(args, 1.0, std::multiplies<double>());
  });

  define_native_function(ctx, "/", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto first_pair = args->require_pair(args);
    double initial = first_pair->left()->require_number(*first_pair->loc());
    auto rest = first_pair->right();
    return *rest
      ? fold_numbers(rest, initial, std::divides<double>())
      : std::make_shared<Number>(1.0 / initial);
  });

  define_native_function(ctx, "mod", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto pair = require_2_numbers(args);
    return std::make_shared<Number>(pair.first - std::floor(pair.first / pair.second) * pair.second);
  });
  define_native_function(ctx, "rem", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto pair = require_2_numbers(args);
    return std::make_shared<Number>(std::fmod(pair.first, pair.second));
  });

  define_native_function(ctx, "sin", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::sin(require_1_number(args)));
  });
  define_native_function(ctx, "asin", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::asin(require_1_number(args)));
  });
  define_native_function(ctx, "cos", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::cos(require_1_number(args)));
  });
  define_native_function(ctx, "acos", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::acos(require_1_number(args)));
  });
  define_native_function(ctx, "tan", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::tan(require_1_number(args)));
  });
  define_native_function(ctx, "atan", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto first_pair = args->require_pair(args);
    auto second_pair = first_pair->right()->as_pair(first_pair->right());
    if (second_pair) {
      second_pair->right()->require_nil();
      return std::make_shared<Number>(std::atan2(
        first_pair->left()->require_number(*first_pair->loc()),
        second_pair->left()->require_number(*second_pair->loc())));

    } else {
      first_pair->right()->require_nil();
      return std::make_shared<Number>(std::tan(first_pair->left()->require_number(*first_pair->loc())));
    }
  });

  define_native_function(ctx, "floor", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::floor(require_1_number(args)));
  });
  define_native_function(ctx, "ceiling", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::ceil(require_1_number(args)));
  });
  define_native_function(ctx, "truncate", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::trunc(require_1_number(args)));
  });
  define_native_function(ctx, "round", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::round(require_1_number(args)));
  });

  define_native_function(ctx, "abs", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::abs(require_1_number(args)));
  });
  define_native_function(ctx, "sqrt", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::sqrt(require_1_number(args)));
  });
  define_native_function(ctx, "exp", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::exp(require_1_number(args)));
  });
  define_native_function(ctx, "expt", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto pair = require_2_numbers(args);
    return std::make_shared<Number>(std::pow(pair.first, pair.second));
  });
  define_native_function(ctx, "log", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto first_pair = args->require_pair(args);
    auto second_pair = first_pair->right()->as_pair(first_pair->right());
    if (second_pair) {
      second_pair->right()->require_nil();
      return std::make_shared<Number>(
        std::log(first_pair->left()->require_number(*first_pair->loc())) /
        std::log(second_pair->left()->require_number(*second_pair->loc())));

    } else {
      first_pair->right()->require_nil();
      return std::make_shared<Number>(std::log(first_pair->left()->require_number(*first_pair->loc())));
    }
  });

  auto not_fn = [=](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return *require_1(args) ? nil_ : t_;
  };
  define_native_function(ctx, "not", not_fn);
  define_native_function(ctx, "null", not_fn);

  define_native_function(ctx, "=", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return compare_numbers(args, std::equal_to<double>());
  });
  define_native_function(ctx, "<", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return compare_numbers(args, std::less<double>());
  });
  define_native_function(ctx, ">", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return compare_numbers(args, std::greater<double>());
  });
  define_native_function(ctx, "<=", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return compare_numbers(args, std::less_equal<double>());
  });
  define_native_function(ctx, ">=", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return compare_numbers(args, std::greater_equal<double>());
  });

  define_native_function(ctx, "/=", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto first_pair = args->require_pair(args);
    std::unordered_set<double> values({first_pair->left()->require_number(*first_pair->loc())});
    auto next = first_pair->right();
    while (*next) {
      auto next_pair = next->require_pair(next);
      if (!values.insert(next_pair->left()->require_number(*next_pair->loc())).second) return Interpreter::nil();
      next = next_pair->right();
    }
    return Interpreter::t();
  });

  define_native_function(ctx, "min", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return reduce_numbers(args, static_cast<double (*)(double, double)>(std::fmin));
  });

  define_native_function(ctx, "max", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return reduce_numbers(args, static_cast<double (*)(double, double)>(std::fmax));
  });

  define_native_function(ctx, "cons", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto pair = require_2(args);
    return std::make_shared<Pair>(pair.first, pair.second);
  });

  define_native_function(ctx, "list", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return args;
  });

  auto car = [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto arg = require_1(args);
    return *arg ? arg->require_pair(arg)->left() : arg;
  };
  define_native_function(ctx, "car", car);
  define_native_function(ctx, "first", car);

  auto cdr = [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto arg = require_1(args);
    return *arg ? arg->require_pair(arg)->right() : arg;
  };
  define_native_function(ctx, "cdr", cdr);
  define_native_function(ctx, "rest", cdr);

  define_native_function(ctx, "nth", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto number_pair = args->require_pair(args);
    auto number = static_cast<int>(number_pair->left()->require_number(*number_pair->loc()));
    auto list_pair = number_pair->right()->require_pair(number_pair->right());
    auto next = list_pair->left();
    list_pair->right()->require_nil();
    while (number >= 0 && *next) {
      auto next_pair = next->require_pair(next);
      if (number-- == 0) return next_pair->left();
      next = next_pair->right();
    }
    return Interpreter::nil();
  });

  define_native_function(ctx, "length", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto list_pair = args->require_pair(args);
    auto next = list_pair->left();
    list_pair->right()->require_nil();
    int length = 0;
    for (; *next; ++length) next = next->require_pair(next)->right();
    return std::make_shared<Number>(length);
  });

  define_native_function(ctx, "append", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto first = nil_;
    std::shared_ptr<Pair> last;
    auto next = args;
    while (*next) {
      auto next_pair = next->require_pair(next);
      auto list_next = next_pair->left();
      while (*list_next) {
        auto list_next_pair = list_next->require_pair(list_next);
        auto new_last = std::make_shared<Pair>(list_next_pair->left(), nil_);
        if (last) last->set_right(new_last);
        else first = new_last;
        last = new_last;
        list_next = list_next_pair->right();
      }
      next = next_pair->right();
    }
    return first;
  });

  define_native_function(ctx, "last", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto first_pair = args->require_pair(args);
    int count = 1;
    if (*first_pair->right()) {
      auto second_pair = first_pair->right()->require_pair(first_pair->right());
      second_pair->right()->require_nil();
      count = static_cast<int>(second_pair->left()->require_number(*second_pair->loc()));
    }
    return last(first_pair->left(), count).first;
  });

  define_native_function(ctx, "reverse", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto first = nil_;
    auto next = require_1(args);
    while (*next) {
      auto next_pair = next->require_pair(next);
      first = std::make_shared<Pair>(next_pair->left(), first);
      next = next_pair->right();
    }
    return first;
  });

  class StaticInterpreter : public Interpreter {
  protected:

    std::shared_ptr<std::string> get_symbol_value (const std::string& name) override {
      return Interpreter::static_symbol_value(name);
    }
  } static_interpreter;

  auto define_macro = [&](const std::string& name, const std::string& definition) {
    auto lambda_def = std::make_shared<LambdaDefinition>(name, static_interpreter, static_interpreter.parse(definition));
    define(ctx, name, std::make_shared<Macro>(lambda_def->evaluate(static_interpreter, lambda_def)));
  };

  define_macro("second",  "((arg) `(nth 1 ,arg))");
  define_macro("third",   "((arg) `(nth 2 ,arg))");
  define_macro("fourth",  "((arg) `(nth 3 ,arg))");
  define_macro("fifth",   "((arg) `(nth 4 ,arg))");
  define_macro("sixth",   "((arg) `(nth 5 ,arg))");
  define_macro("seventh", "((arg) `(nth 6 ,arg))");
  define_macro("eighth",  "((arg) `(nth 7 ,arg))");
  define_macro("ninth",   "((arg) `(nth 8 ,arg))");
  define_macro("tenth",   "((arg) `(nth 9 ,arg))");

  for (int length = 2; length <= 4; ++length) {
    auto name = 'c' + std::string(length, 'a') + 'r';

    std::string definition("((arg) `");
    for (int i = 0; i < length; ++i) definition += "(car ";
    definition += ",arg)" + std::string(length, ')');

    for (int pattern = 0; pattern < (1 << length); ++pattern) {
      for (int pos = 0; pos < length; ++pos) {
        auto ch = (pattern & (1 << (length - pos - 1))) ? 'd' : 'a';
        name[1 + pos] = ch;
        definition[8 + pos * 5 + 2] = ch;
      }
      define_macro(name, definition);
    }
  }

  define_macro("let", "((&rest args) `((lambda (&aux ,@(first args)) ,@(rest args))))");

  return true;
}

}
