#include <chrono>
#include <cmath>
#include <functional>
#include <sstream>
#include <unordered_set>
#include <utility>

#include "goldberg.hpp"

namespace goldberg {

namespace {

inline void bind_value (bindings& static_bindings, const std::string& name, const std::shared_ptr<Value>& value) {
  static_bindings[Interpreter::static_symbol_value(name)] = value;
}

template<typename Expand>
inline void bind_expander (bindings& static_bindings, const std::string& name, const Expand& expand) {
  bind_value(static_bindings, name, std::make_shared<Expander<Expand>>(name, expand));
}

template<typename Invoke>
inline void bind_operator (bindings& static_bindings, const std::string& name, const Invoke& invoke) {
  bind_value(static_bindings, name, std::make_shared<Operator<Invoke>>(name, invoke));
}

template<typename Expand, typename Invoke>
inline void bind_expand_operator (
    bindings& static_bindings, const std::string& name, const Expand& expand, const Invoke& invoke) {
  bind_value(static_bindings, name, std::make_shared<ExpandOperator<Expand, Invoke>>(name, expand, invoke));
}

template<typename Apply>
inline void bind_native_function (bindings& static_bindings, const std::string& name, const Apply& apply) {
  bind_value(static_bindings, name, std::make_shared<NativeFunction<Apply>>(name, apply));
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

bool Interpreter::populate_statics () {
  bind_value(static_bindings_, "nil", nil_);
  bind_value(static_bindings_, "t", t_);
  bind_value(static_bindings_, "pi", std::make_shared<Number>(M_PI));

  bind_expand_operator(
    static_bindings_, "quote",
    [](Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) {
      return std::make_shared<Pair>(self, pair.right(), pair.loc());
    },
    [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
      return require_1(args);
    });

  bind_expand_operator(
    static_bindings_, "backquote",
    [](Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) {
      return std::make_shared<Pair>(self, pair.right()->compile_commas(interpreter, pair.right()), pair.loc());
    },
    [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
      auto arg = require_1(args);
      return arg->evaluate_commas(interpreter, arg);
    });

  bind_expand_operator(
    static_bindings_, "defconstant",
    [](Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) {
      auto symbol_pair = pair.right()->require_pair(pair.right());
      auto symbol = symbol_pair->left();
      auto symbol_value = symbol->require_symbol(*symbol_pair->loc());
      auto value_pair = symbol_pair->right()->require_pair(symbol_pair->right());

      interpreter.top_level_bindings()[symbol_value] = std::make_shared<ConstantVariable>(symbol_value);

      auto new_value = value_pair->left()->compile(interpreter, value_pair->left());
      value_pair->right()->require_nil();

      return std::make_shared<Pair>(
        self,
        std::make_shared<Pair>(symbol, std::make_shared<Pair>(new_value, nil_)),
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

  bind_expand_operator(
    static_bindings_, "defparameter",
    [](Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) {
      auto symbol_pair = pair.right()->require_pair(pair.right());
      auto symbol = symbol_pair->left();
      auto symbol_value = symbol->require_symbol(*symbol_pair->loc());
      auto value_pair = symbol_pair->right()->require_pair(symbol_pair->right());

      interpreter.top_level_bindings()[symbol_value] = std::make_shared<DynamicVariable>(symbol_value);

      auto new_value = value_pair->left()->compile(interpreter, value_pair->left());
      value_pair->right()->require_nil();

      return std::make_shared<Pair>(
        self,
        std::make_shared<Pair>(symbol, std::make_shared<Pair>(new_value, nil_)),
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

  bind_expand_operator(
    static_bindings_, "defvar",
    [](Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) {
      auto symbol_pair = pair.right()->require_pair(pair.right());
      auto symbol = symbol_pair->left();
      auto symbol_value = symbol->require_symbol(*symbol_pair->loc());

      interpreter.top_level_bindings()[symbol_value] = std::make_shared<DynamicVariable>(symbol_value);

      auto value_pair = symbol_pair->right()->as_pair(symbol_pair->right());
      auto new_value_pair = nil_;
      if (value_pair) {
        auto new_value = value_pair->left()->compile(interpreter, value_pair->left());
        value_pair->right()->require_nil();
        new_value_pair = std::make_shared<Pair>(new_value, nil_);

      } else symbol_pair->right()->require_nil();

      return std::make_shared<Pair>(self, std::make_shared<Pair>(symbol, new_value_pair), pair.loc());
    },
    [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
      auto symbol_pair = args->require_pair(args);
      auto symbol = symbol_pair->left();
      auto symbol_value = symbol->require_symbol(*symbol_pair->loc());

      auto value_pair = symbol_pair->right()->as_pair(symbol_pair->right());
      auto new_value = nil_;
      if (value_pair) {
        new_value = value_pair->left()->evaluate(interpreter, value_pair->left());
        value_pair->right()->require_nil();
      }

      if (!interpreter.top_level_context()->is_defined(symbol_value)) {
        interpreter.top_level_context()->define(symbol_value, new_value);
      }

      return symbol;
    });

  bind_expand_operator(
    static_bindings_, "defun",
    [](Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) {
      auto symbol_pair = pair.right()->require_pair(pair.right());
      auto symbol = symbol_pair->left();
      auto symbol_value = symbol->require_symbol(*symbol_pair->loc());

      interpreter.top_level_bindings()[symbol_value] = std::make_shared<LexicalVariable>(symbol_value);

      auto new_value = std::make_shared<LambdaDefinition>(*symbol_value, interpreter, symbol_pair->right());

      return std::make_shared<Pair>(
        self,
        std::make_shared<Pair>(symbol, std::make_shared<Pair>(new_value, nil_)),
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

  bind_expand_operator(
    static_bindings_, "defmacro",
    [](Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) {
      auto symbol_pair = pair.right()->require_pair(pair.right());
      auto symbol = symbol_pair->left();
      auto symbol_value = symbol->require_symbol(*symbol_pair->loc());
      auto new_value = std::make_shared<LambdaDefinition>(*symbol_value, interpreter, symbol_pair->right());

      return std::make_shared<Pair>(
        self,
        std::make_shared<Pair>(symbol, std::make_shared<Pair>(new_value, nil_)),
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

  bind_expander(
    static_bindings_, "lambda",
    [](Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) {
      return std::make_shared<LambdaDefinition>("lambda", interpreter, pair.right());
    });

  bind_operator(static_bindings_, "if", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
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

  bind_operator(static_bindings_, "cond", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto next = args;
    while (*next) {
      auto next_pair = next->require_pair(next);
      auto clause_pair = next_pair->left()->require_pair(next_pair->left());
      next = next_pair->right();
      auto last_result = clause_pair->left()->evaluate(interpreter, clause_pair->left());
      if (*last_result) {
        next = clause_pair->right();
        while (*next) {
          next_pair = next->require_pair(next);
          last_result = next_pair->left()->evaluate(interpreter, next_pair->left());
          next = next_pair->right();
        }
        return last_result;
      }
    }
    return nil_;
  });

  bind_operator(static_bindings_, "and", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
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

  bind_operator(static_bindings_, "or", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto next = args;
    while (*next) {
      auto next_pair = next->require_pair(next);
      auto value = next_pair->left()->evaluate(interpreter, next_pair->left());
      if (*value) return value;
      next = next_pair->right();
    }
    return nil_;
  });

  bind_operator(static_bindings_, "progn", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto next = args;
    auto last_result = nil_;
    while (*next) {
      auto next_pair = next->require_pair(next);
      last_result = next_pair->left()->evaluate(interpreter, next_pair->left());
      next = next_pair->right();
    }
    return last_result;
  });

  auto setq = [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto next = args;
    auto last_result = nil_;
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
  };
  bind_operator(static_bindings_, "setq", setq);
  bind_operator(static_bindings_, "setf", setq);

  bind_native_function(static_bindings_, "eval", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto arg = require_1(args);
    auto compiled = arg->compile(interpreter, arg);
    return compiled->evaluate(interpreter, compiled);
  });

  bind_native_function(static_bindings_, "apply", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto function_pair = args->require_pair(args);
    auto arg_pair = function_pair->right()->require_pair(function_pair->right());
    auto first = nil_;
    std::shared_ptr<Pair> last;
    while (*arg_pair->right()) {
      auto new_last = std::make_shared<Pair>(arg_pair->left(), nil_);
      if (last) last->set_right(new_last);
      else first = new_last;
      last = new_last;
      arg_pair = arg_pair->right()->require_pair(arg_pair->right());
    }
    if (last) last->set_right(arg_pair->left());
    else first = arg_pair->left();
    return function_pair->left()->apply(interpreter, first, *function_pair->loc());
  });

  bind_native_function(static_bindings_, "mapcar", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto function_pair = args->require_pair(args);
    auto function = function_pair->left();
    auto next_list_pair = function_pair->right()->require_pair(function_pair->right());

    std::vector<std::shared_ptr<Value>> arg_lists{next_list_pair->left()};
    auto last_arg = std::make_shared<Pair>(nil_, nil_);
    std::shared_ptr<Value> first_arg = last_arg;

    while (*next_list_pair->right()) {
      next_list_pair = next_list_pair->right()->require_pair(next_list_pair->right());
      arg_lists.push_back(next_list_pair->left());
      auto new_last_arg = std::make_shared<Pair>(nil_, nil_);
      last_arg->set_right(new_last_arg);
      last_arg = new_last_arg;
    }

    auto first_result = nil_;
    std::shared_ptr<Pair> last_result;

    while (true) {
      auto next_arg = first_arg;
      for (auto& next_element : arg_lists) {
        if (!*next_element) return first_result;
        auto next_element_pair = next_element->require_pair(next_element);
        next_element = next_element_pair->right();
        auto next_arg_pair = next_arg->require_pair(next_arg);
        next_arg_pair->set_left(next_element_pair->left());
        next_arg = next_arg_pair->right();
      }
      auto new_last_result = std::make_shared<Pair>(function->apply(interpreter, first_arg, *function_pair->loc()), nil_);
      if (last_result) last_result->set_right(new_last_result);
      else first_result = new_last_result;
      last_result = new_last_result;
    }
  });

  bind_native_function(static_bindings_, "load", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return interpreter.load(require_1(args)->require_string(*args->loc()));
  });
  bind_native_function(static_bindings_, "require", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return interpreter.require(require_1(args)->require_string(*args->loc()));
  });

  bind_native_function(static_bindings_, "eq", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto pair = require_2(args);
    return pair.first == pair.second ? t_ : nil_;
  });
  bind_native_function(static_bindings_, "equal", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto pair = require_2(args);
    return pair.first->equals(pair.second) ? t_ : nil_;
  });

  bind_native_function(static_bindings_, "+", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return fold_numbers(args, 0.0, std::plus<double>());
  });

  bind_native_function(static_bindings_, "-", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto first_pair = args->require_pair(args);
    double initial = first_pair->left()->require_number(*first_pair->loc());
    auto rest = first_pair->right();
    return *rest
      ? fold_numbers(rest, initial, std::minus<double>())
      : std::make_shared<Number>(-initial);
  });

  bind_native_function(static_bindings_, "*", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return fold_numbers(args, 1.0, std::multiplies<double>());
  });

  bind_native_function(static_bindings_, "/", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto first_pair = args->require_pair(args);
    double initial = first_pair->left()->require_number(*first_pair->loc());
    auto rest = first_pair->right();
    return *rest
      ? fold_numbers(rest, initial, std::divides<double>())
      : std::make_shared<Number>(1.0 / initial);
  });

  bind_native_function(static_bindings_, "1+", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(require_1_number(args) + 1);
  });
  bind_native_function(static_bindings_, "1-", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(require_1_number(args) - 1);
  });

  bind_native_function(static_bindings_, "mod", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto pair = require_2_numbers(args);
    return std::make_shared<Number>(pair.first - std::floor(pair.first / pair.second) * pair.second);
  });
  bind_native_function(static_bindings_, "rem", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto pair = require_2_numbers(args);
    return std::make_shared<Number>(std::fmod(pair.first, pair.second));
  });

  bind_native_function(static_bindings_, "sin", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::sin(require_1_number(args)));
  });
  bind_native_function(static_bindings_, "asin", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::asin(require_1_number(args)));
  });
  bind_native_function(static_bindings_, "cos", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::cos(require_1_number(args)));
  });
  bind_native_function(static_bindings_, "acos", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::acos(require_1_number(args)));
  });
  bind_native_function(static_bindings_, "tan", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::tan(require_1_number(args)));
  });
  bind_native_function(static_bindings_, "atan", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
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

  bind_native_function(static_bindings_, "floor", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::floor(require_1_number(args)));
  });
  bind_native_function(static_bindings_, "ceiling", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::ceil(require_1_number(args)));
  });
  bind_native_function(static_bindings_, "truncate", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::trunc(require_1_number(args)));
  });
  bind_native_function(static_bindings_, "round", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::round(require_1_number(args)));
  });

  bind_native_function(static_bindings_, "abs", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::abs(require_1_number(args)));
  });
  bind_native_function(static_bindings_, "sqrt", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::sqrt(require_1_number(args)));
  });
  bind_native_function(static_bindings_, "exp", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Number>(std::exp(require_1_number(args)));
  });
  bind_native_function(static_bindings_, "expt", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto pair = require_2_numbers(args);
    return std::make_shared<Number>(std::pow(pair.first, pair.second));
  });
  bind_native_function(static_bindings_, "log", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
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

  auto not_fn = [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return *require_1(args) ? nil_ : t_;
  };
  bind_native_function(static_bindings_, "not", not_fn);
  bind_native_function(static_bindings_, "null", not_fn);

  bind_native_function(static_bindings_, "=", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return compare_numbers(args, std::equal_to<double>());
  });
  bind_native_function(static_bindings_, "<", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return compare_numbers(args, std::less<double>());
  });
  bind_native_function(static_bindings_, ">", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return compare_numbers(args, std::greater<double>());
  });
  bind_native_function(static_bindings_, "<=", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return compare_numbers(args, std::less_equal<double>());
  });
  bind_native_function(static_bindings_, ">=", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return compare_numbers(args, std::greater_equal<double>());
  });

  bind_native_function(static_bindings_, "/=", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto first_pair = args->require_pair(args);
    std::unordered_set<double> values({first_pair->left()->require_number(*first_pair->loc())});
    auto next = first_pair->right();
    while (*next) {
      auto next_pair = next->require_pair(next);
      if (!values.insert(next_pair->left()->require_number(*next_pair->loc())).second) return nil_;
      next = next_pair->right();
    }
    return t_;
  });

  bind_native_function(static_bindings_, "min", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return reduce_numbers(args, static_cast<double (*)(double, double)>(std::fmin));
  });

  bind_native_function(static_bindings_, "max", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return reduce_numbers(args, static_cast<double (*)(double, double)>(std::fmax));
  });

  bind_native_function(static_bindings_, "cons", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto pair = require_2(args);
    return std::make_shared<Pair>(pair.first, pair.second);
  });

  bind_native_function(static_bindings_, "list", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return args;
  });

  auto car = [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto arg = require_1(args);
    return *arg ? arg->require_pair(arg)->left() : arg;
  };
  bind_native_function(static_bindings_, "car", car);
  bind_native_function(static_bindings_, "first", car);

  auto cdr = [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto arg = require_1(args);
    return *arg ? arg->require_pair(arg)->right() : arg;
  };
  bind_native_function(static_bindings_, "cdr", cdr);
  bind_native_function(static_bindings_, "rest", cdr);

  bind_native_function(static_bindings_, "nth", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
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
    return nil_;
  });

  bind_native_function(static_bindings_, "length", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto list_pair = args->require_pair(args);
    auto next = list_pair->left();
    list_pair->right()->require_nil();
    int length = 0;
    for (; *next; ++length) next = next->require_pair(next)->right();
    return std::make_shared<Number>(length);
  });

  bind_native_function(static_bindings_, "append", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto first = nil_;
    std::shared_ptr<Pair> last;
    auto next = args;
    while (*next) {
      auto next_pair = next->require_pair(next);
      auto list_next = next_pair->left();
      next = next_pair->right();
      if (!*next) { // use the last as-is
        if (last) last->set_right(list_next);
        else first = list_next;
        break;
      }
      while (*list_next) {
        auto list_next_pair = list_next->require_pair(list_next);
        auto new_last = std::make_shared<Pair>(list_next_pair->left(), nil_);
        if (last) last->set_right(new_last);
        else first = new_last;
        last = new_last;
        list_next = list_next_pair->right();
      }
    }
    return first;
  });

  bind_native_function(static_bindings_, "last", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto first_pair = args->require_pair(args);
    int count = 1;
    if (*first_pair->right()) {
      auto second_pair = first_pair->right()->require_pair(first_pair->right());
      second_pair->right()->require_nil();
      count = static_cast<int>(second_pair->left()->require_number(*second_pair->loc()));
    }
    return last(first_pair->left(), count).first;
  });

  bind_native_function(static_bindings_, "reverse", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto first = nil_;
    auto next = require_1(args);
    while (*next) {
      auto next_pair = next->require_pair(next);
      first = std::make_shared<Pair>(next_pair->left(), first);
      next = next_pair->right();
    }
    return first;
  });

  bind_native_function(static_bindings_, "member", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto item_pair = args->require_pair(args);
    auto item = item_pair->left();
    auto list_pair = item_pair->right()->require_pair(item_pair->right());
    auto next = list_pair->left();
    list_pair->right()->require_nil();

    while (*next) {
      auto next_pair = next->require_pair(next);
      if (next_pair->left()->equals(item)) return next;
      next = next_pair->right();
    }
    return nil_;
  });

  bind_native_function(static_bindings_, "remove-if-not", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto predicate_pair = args->require_pair(args);
    auto arg_pair = std::make_shared<Pair>(nil_, nil_);
    auto list_pair = predicate_pair->right()->require_pair(predicate_pair->right());
    list_pair->right()->require_nil();

    auto first = nil_;
    std::shared_ptr<Pair> last;
    auto next = list_pair->left();
    while (*next) {
      auto next_pair = next->require_pair(next);
      arg_pair->set_left(next_pair->left());
      if (*predicate_pair->left()->apply(interpreter, arg_pair, *predicate_pair->loc())) {
        auto new_last = std::make_shared<Pair>(next_pair->left(), nil_);
        if (last) last->set_right(new_last);
        else first = new_last;
        last = new_last;
      }
      next = next_pair->right();
    }

    return first;
  });

  bind_native_function(static_bindings_, "consp", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto arg = require_1(args);
    return arg->as_pair(arg) ? t_ : nil_;
  });
  bind_native_function(static_bindings_, "listp", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto arg = require_1(args);
    return arg->is_nil() || arg->as_pair(arg) ? t_ : nil_;
  });

  auto string_symbol = Interpreter::static_symbol_value("string");
  auto list_symbol = Interpreter::static_symbol_value("list");
  bind_native_function(static_bindings_, "concatenate", [=](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto type_pair = args->require_pair(args);
    auto type = type_pair->left()->require_symbol(*type_pair->loc());
    auto next = type_pair->right();
    if (type == string_symbol) {
      std::string value;
      while (*next) {
        auto next_pair = next->require_pair(next);
        value += next_pair->left()->require_string(*next_pair->loc());
        next = next_pair->right();
      }
      return std::static_pointer_cast<Value>(std::make_shared<String>(value));

    } else if (type == list_symbol) {
      auto first = nil_;
      std::shared_ptr<Pair> last;
      while (*next) {
        auto next_pair = next->require_pair(next);
        auto list_next = next_pair->left();
        next = next_pair->right();

        while (*list_next) {
          auto list_next_pair = list_next->require_pair(list_next);
          auto new_last = std::make_shared<Pair>(list_next_pair->left(), nil_);
          if (last) last->set_right(new_last);
          else first = new_last;
          last = new_last;
          list_next = list_next_pair->right();
        }
      }
      return first;

    } else throw script_error("Unknown type for concatenate \"" + *type + "\"", *type_pair->loc());
  });

  bind_native_function(static_bindings_, "write-to-string", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<String>(require_1(args)->to_string());
  });

  bind_native_function(static_bindings_, "format", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto destination_pair = args->require_pair(args);
    auto control_string_pair = destination_pair->right()->require_pair(destination_pair->right());
    auto& control_string = control_string_pair->left()->require_string(*control_string_pair->loc());
    auto next_arg = control_string_pair->right();

    std::ostringstream out;
    for (auto it = control_string.begin(); it != control_string.end(); ) {
      char ch = *it++;
      if (ch != '~') {
        out << ch;
        continue;
      }
      if (it == control_string.end()) break;
      switch (*it++) {
        case 'a':
        case 'A': {
          auto next_pair = next_arg->require_pair(next_arg);
          out << next_pair->left()->to_raw_string();
          next_arg = next_pair->right();
          break;
        }

        case '~':
          out << '~';
          break;
      }
    }
    return std::make_shared<String>(out.str());
  });

  bind_native_function(static_bindings_, "make-symbol", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return std::make_shared<Symbol>(interpreter.get_symbol_value(require_1(args)->require_string(*args->loc())));
  });

  auto random_state_symbol = Interpreter::static_symbol_value("*random-state*");
  bind_native_function(
      static_bindings_, "make-random-state", [=](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto state = nil_;
    if (*args) {
      auto arg_pair = args->require_pair(args);
      state = arg_pair->left();
      arg_pair->right()->require_nil();
    }
    if (!*state) state = interpreter.lookup_dynamic_value(random_state_symbol);

    std::default_random_engine engine;
    if (state == t_) engine = std::default_random_engine(std::chrono::system_clock::now().time_since_epoch().count());
    else {
      auto number = state->as_number();
      if (!std::isnan(number)) {
        engine = std::default_random_engine(static_cast<std::default_random_engine::result_type>(number));
      } else {
        engine = state->require_random_state(*state->loc());
      }
    }

    return std::make_shared<RandomState>("RandomState", engine);
  });

  bind_native_function(static_bindings_, "random", [=](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto arg_pair = args->require_pair(args);
    auto limit = arg_pair->left()->require_number(*arg_pair->loc());
    auto state_pair = arg_pair->right()->as_pair(arg_pair->right());
    std::shared_ptr<Value> state;
    if (state_pair) {
      state = state_pair->left();
      state_pair->right()->require_nil();

    } else state = interpreter.lookup_dynamic_value(random_state_symbol);

    return std::make_shared<Number>(std::uniform_real_distribution<>(0.0, limit)(state->require_random_state(*state->loc())));
  });

  class StaticInterpreter : public Interpreter {
  protected:

    std::shared_ptr<std::string> get_symbol_value (const std::string& name) override {
      return Interpreter::static_symbol_value(name);
    }
  } static_interpreter;

  auto bind_macro = [&](const std::string& name, const std::string& definition) {
    auto lambda_def = std::make_shared<LambdaDefinition>(name, static_interpreter, static_interpreter.parse(definition));
    bind_value(static_bindings_, name, std::make_shared<Macro>(lambda_def->evaluate(static_interpreter, lambda_def)));
  };

  bind_macro("second",  "((arg) `(nth 1 ,arg))");
  bind_macro("third",   "((arg) `(nth 2 ,arg))");
  bind_macro("fourth",  "((arg) `(nth 3 ,arg))");
  bind_macro("fifth",   "((arg) `(nth 4 ,arg))");
  bind_macro("sixth",   "((arg) `(nth 5 ,arg))");
  bind_macro("seventh", "((arg) `(nth 6 ,arg))");
  bind_macro("eighth",  "((arg) `(nth 7 ,arg))");
  bind_macro("ninth",   "((arg) `(nth 8 ,arg))");
  bind_macro("tenth",   "((arg) `(nth 9 ,arg))");

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
      bind_macro(name, definition);
    }
  }

  bind_macro("let", "((bindings &rest body) `((lambda (&aux ,@bindings) ,@body)))");
  bind_macro("let*",
    "((bindings &rest body)"
    "  `((lambda (&aux ,@(mapcar (lambda (b) (if (consp b) (car b) b)) bindings))"
    "    (setq ,@(apply append (remove-if-not consp bindings)))"
    "    ,@body)))");

  bind_macro("incf", "((var) `(setf ,var (1+ ,var)))");
  bind_macro("decf", "((var) `(setf ,var (1- ,var)))");

  auto bind_dynamic_variable = [&](const std::string& name) {
    bind_value(static_bindings_, name, std::make_shared<DynamicVariable>(Interpreter::static_symbol_value(name)));
  };

  auto define_function = [&](const std::string& name, const std::string& definition) {
    auto symbol_value = Interpreter::static_symbol_value(name);
    bind_value(static_bindings_, name, std::make_shared<LexicalVariable>(symbol_value));
    auto lambda_def = std::make_shared<LambdaDefinition>(name, static_interpreter, static_interpreter.parse(definition));
    static_context_->define(symbol_value, lambda_def->evaluate(static_interpreter, lambda_def));
  };

  bind_dynamic_variable("*gensym-counter*");
  define_function("gensym", "((&optional (prefix \"G\")) (make-symbol (format nil \"~a~a\" prefix (incf *gensym-counter*))))");

  bind_macro("case",
    "((keyform &rest clauses &aux (keyvar (gensym)))"
    "  `(let ((,keyvar ,keyform)) (cond"
    "    ,@(mapcar"
    "        (lambda (clause) `("
    "          ,(let ((keys (first clause))) (cond"
    "            ((listp keys) `(member ,keyvar ',keys))"
    "            ((or (equal keys 't) (equal keys 'otherwise)) 't)"
    "            (t `(equal ,keyvar ',keys))))"
    "          ,@(rest clause)"
    "        ))"
    "      clauses))))");

  define_function("evenp", "((v) (= (mod v 2) 0))");
  define_function("oddp", "((v) (= (mod v 2) 1))");

  define_function("member-if",
    "((pred arg)"
    "  (cond"
    "    ((null arg) nil)"
    "    ((pred (car arg)) arg)"
    "    (t (member-if pred (cdr arg)))))");

  return true;
}

}
