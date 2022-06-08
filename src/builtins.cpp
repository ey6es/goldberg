#include "goldberg.hpp"

namespace goldberg {

inline std::shared_ptr<Value> require_1 (const std::shared_ptr<Value>& args) {
  auto pair = args->require_pair(args);
  pair->right()->require_nil();
  return pair->left();
}

template<typename T>
inline std::shared_ptr<Value> fold_numbers (const std::shared_ptr<Value>& args, double initial, const T& fold) {
  double value = initial;
  auto next = args;
  while (*next) {
    auto next_pair = next->require_pair(next);
    value = fold(value, next_pair->left()->require_number(*next_pair->loc()));
    next = next_pair->right();
  }
  return std::shared_ptr<Value>(new Number(value));
}

void Interpreter::register_builtins () {
  std::shared_ptr<Value> nil(new Nil());
  register_builtin("nil", nil);
  std::shared_ptr<Value> t(new True());
  register_builtin("t", t);

  register_native_operator("quote", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return require_1(args);
  });

  register_native_operator("if", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
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

  register_native_operator("and", [=](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto value = t;
    auto next = args;
    while (*next) {
      auto next_pair = next->require_pair(next);
      value = next_pair->left()->evaluate(interpreter, next_pair->left());
      if (!*value) return nil;
      next = next_pair->right();
    }
    return value;
  });

  register_native_operator("or", [=](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto next = args;
    while (*next) {
      auto next_pair = next->require_pair(next);
      auto value = next_pair->left()->evaluate(interpreter, next_pair->left());
      if (*value) return value;
      next = next_pair->right();
    }
    return nil;
  });

  register_native_function("+", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return fold_numbers(args, 0.0, [](double acc, double value) { return acc + value; });
  });

  register_native_function("-", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto first_pair = args->require_pair(args);
    double initial = first_pair->left()->require_number(*first_pair->loc());
    auto rest = first_pair->right();
    return *rest
      ? fold_numbers(rest, initial, [](double acc, double value) { return acc - value; })
      : std::shared_ptr<Value>(new Number(-initial));
  });

  register_native_function("*", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return fold_numbers(args, 1.0, [](double acc, double value) { return acc * value; });
  });

  register_native_function("/", [](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    auto first_pair = args->require_pair(args);
    double initial = first_pair->left()->require_number(*first_pair->loc());
    auto rest = first_pair->right();
    return *rest
      ? fold_numbers(rest, initial, [](double acc, double value) { return acc / value; })
      : std::shared_ptr<Value>(new Number(1.0 / initial));
  });

  auto not_fn = [=](Interpreter& interpreter, const std::shared_ptr<Value>& args) {
    return *require_1(args) ? nil : t;
  };
  register_native_function("not", not_fn);
  register_native_function("null", not_fn);
}

}
