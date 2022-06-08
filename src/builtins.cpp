#include <functional>
#include <unordered_set>

#include "goldberg.hpp"

namespace goldberg {

namespace {

inline void add_value (const std::shared_ptr<invocation>& ctx, const std::string& name, const std::shared_ptr<Value>& value) {
  ctx->lexical_vars[Interpreter::static_symbol_value(name)] = value;
}

template<typename T>
inline void add_native_operator (const std::shared_ptr<invocation>& ctx, const std::string& name, T function) {
  add_value(ctx, name, std::shared_ptr<Value>(new NativeOperator<T>(name, function)));
}

template<typename T>
inline void add_native_function (const std::shared_ptr<invocation>& ctx, const std::string& name, T function) {
  add_value(ctx, name, std::shared_ptr<Value>(new NativeFunction<T>(name, function)));
}

inline std::shared_ptr<Value> require_1 (const std::shared_ptr<Value>& args) {
  auto pair = args->require_pair(args);
  pair->right()->require_nil();
  return pair->left();
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
  return std::shared_ptr<Value>(new Number(value));
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

}

std::shared_ptr<invocation> Interpreter::create_builtin_context () {
  auto ctx = std::make_shared<invocation>();

  add_value(ctx, "nil", nil_);
  add_value(ctx, "t", t_);

  add_native_operator(ctx, "quote", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return require_1(args);
  });

  add_native_operator(ctx, "if", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
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

  add_native_operator(ctx, "and", [=](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
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

  add_native_operator(ctx, "or", [=](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto next = args;
    while (*next) {
      auto next_pair = next->require_pair(next);
      auto value = next_pair->left()->evaluate(interpreter, next_pair->left());
      if (*value) return value;
      next = next_pair->right();
    }
    return nil_;
  });

  add_native_function(ctx, "+", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return fold_numbers(args, 0.0, std::plus<double>());
  });

  add_native_function(ctx, "-", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto first_pair = args->require_pair(args);
    double initial = first_pair->left()->require_number(*first_pair->loc());
    auto rest = first_pair->right();
    return *rest
      ? fold_numbers(rest, initial, std::minus<double>())
      : std::shared_ptr<Value>(new Number(-initial));
  });

  add_native_function(ctx, "*", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return fold_numbers(args, 1.0, std::multiplies<double>());
  });

  add_native_function(ctx, "/", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto first_pair = args->require_pair(args);
    double initial = first_pair->left()->require_number(*first_pair->loc());
    auto rest = first_pair->right();
    return *rest
      ? fold_numbers(rest, initial, std::divides<double>())
      : std::shared_ptr<Value>(new Number(1.0 / initial));
  });

  auto not_fn = [=](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return *require_1(args) ? nil_ : t_;
  };
  add_native_function(ctx, "not", not_fn);
  add_native_function(ctx, "null", not_fn);

  add_native_function(ctx, "=", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return compare_numbers(args, std::equal_to<double>());
  });
  add_native_function(ctx, "<", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return compare_numbers(args, std::less<double>());
  });
  add_native_function(ctx, ">", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return compare_numbers(args, std::greater<double>());
  });
  add_native_function(ctx, "<=", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return compare_numbers(args, std::less_equal<double>());
  });
  add_native_function(ctx, ">=", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return compare_numbers(args, std::greater_equal<double>());
  });

  add_native_function(ctx, "/=", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
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

  return ctx;
}

}
