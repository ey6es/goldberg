#include <cmath>
#include <functional>
#include <unordered_set>
#include <utility>

#include "goldberg.hpp"

namespace goldberg {

namespace {

inline void define (const std::shared_ptr<Invocation>& ctx, const std::string& name, const std::shared_ptr<Value>& value) {
  ctx->define(Interpreter::static_symbol_value(name), value);
}

template<typename T>
inline void define_native_operator (const std::shared_ptr<Invocation>& ctx, const std::string& name, T function) {
  define(ctx, name, std::make_shared<NativeOperator<T>>(name, function));
}

template<typename T>
inline void define_native_function (const std::shared_ptr<Invocation>& ctx, const std::string& name, T function) {
  define(ctx, name, std::make_shared<NativeFunction<T>>(name, function));
}

inline std::shared_ptr<Value> require_1 (const std::shared_ptr<Value>& args) {
  auto pair = args->require_pair(args);
  pair->right()->require_nil();
  return pair->left();
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

std::shared_ptr<Invocation> Interpreter::create_builtin_context () {
  auto ctx = std::make_shared<Invocation>();

  define(ctx, "nil", nil_);
  define(ctx, "t", t_);

  define_native_operator(ctx, "quote", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return require_1(args);
  });

  define_native_operator(ctx, "if", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
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

  define_native_operator(ctx, "and", [=](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
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

  define_native_operator(ctx, "or", [=](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto next = args;
    while (*next) {
      auto next_pair = next->require_pair(next);
      auto value = next_pair->left()->evaluate(interpreter, next_pair->left());
      if (*value) return value;
      next = next_pair->right();
    }
    return nil_;
  });

  define_native_operator(ctx, "lambda", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto arg_pair = args->require_pair(args);
    return std::make_shared<Lambda>(
      "lambda",
      interpreter.current_context(),
      parameters(interpreter, arg_pair->left()),
      arg_pair->right());
  });

  define_native_function(ctx, "eval", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto arg = require_1(args);
    return arg->evaluate(interpreter, arg);
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
    return arg->require_pair(arg)->left();
  };
  define_native_function(ctx, "car", car);
  define_native_function(ctx, "first", car);

  auto cdr = [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto arg = require_1(args);
    return arg->require_pair(arg)->right();
  };
  define_native_function(ctx, "cdr", cdr);
  define_native_function(ctx, "rest", cdr);

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

  return ctx;
}

}
