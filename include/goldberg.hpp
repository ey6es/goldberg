#ifndef GOLDBERG_H
#define GOLDBERG_H

#include <cmath>
#include <exception>
#include <istream>
#include <memory>
#include <random>
#include <string>
#include <unordered_map>
#include <vector>

namespace goldberg {

class Invocation;
class Value;
struct lexeme;
struct location;

typedef std::unordered_map<std::shared_ptr<std::string>, std::shared_ptr<Value>> bindings;

class Interpreter {
public:

  static const std::shared_ptr<Value>& t () { return t_; }
  static const std::shared_ptr<Value>& nil () { return nil_; }

  static std::shared_ptr<std::string> static_symbol_value (const std::string& name);

  Interpreter ();
  virtual ~Interpreter () {}

  const std::shared_ptr<Invocation>& current_context () const { return call_stack_.back(); }

  std::shared_ptr<Value> require (const std::string& filename);
  std::shared_ptr<Value> load (const std::string& filename);

  std::shared_ptr<Value> parse (const std::string& string, const std::string& filename = "<string>");
  std::shared_ptr<Value> parse (std::istream& in, const std::string& filename);

  std::shared_ptr<Value> evaluate (const std::string& string, const std::string& filename = "<string>");
  std::shared_ptr<Value> evaluate (std::istream& in, const std::string& filename);

protected:

  virtual std::shared_ptr<std::string> get_symbol_value (const std::string& name);

private:

  friend class Symbol;
  friend class LambdaDefinition;
  friend class LambdaFunction;
  friend class DynamicVariable;

  static std::shared_ptr<Value> t_;
  static std::shared_ptr<Value> nil_;

  static std::unordered_map<std::string, std::shared_ptr<std::string>> static_symbol_values_;

  static std::shared_ptr<std::string> random_state_symbol_;

  static bindings static_bindings_;
  static bool static_bindings_populated_;

  static bool populate_static_bindings ();

  std::shared_ptr<Value> parse (std::istream& in, location& loc);
  std::shared_ptr<Value> parse (std::istream& in, location& loc, const lexeme& token);
  std::shared_ptr<Value> parse_rest (std::istream& in, location& loc);

  lexeme lex (std::istream& in, location& loc);

  void push_bindings (bindings&& ctx) { binding_stack_.push_back(ctx); }
  void pop_bindings () { binding_stack_.pop_back(); }

  bindings& top_level_bindings () { return binding_stack_.front(); }

  std::shared_ptr<Value> lookup_binding (const std::shared_ptr<std::string>& symbol_value) const;

  void push_frame (const std::shared_ptr<Invocation>& ctx) { call_stack_.push_back(ctx); }
  void pop_frame () { call_stack_.pop_back(); }

  const std::shared_ptr<Invocation>& top_level_context () const { return call_stack_.front(); }

  std::shared_ptr<Value> lookup_dynamic_value (const std::shared_ptr<std::string>& symbol_value) const;
  void set_dynamic_value (const std::shared_ptr<std::string>& symbol_value, const std::shared_ptr<Value>& value) const;

  std::unordered_map<std::string, std::weak_ptr<std::string>> symbol_values_;
  std::vector<bindings> binding_stack_;
  std::vector<std::shared_ptr<Invocation>> call_stack_;
  std::unordered_map<std::string, std::shared_ptr<Value>> required_files_;
};

class Pair;

class Value {
public:

  explicit Value (const std::shared_ptr<location>& loc = nullptr) : loc_(loc) {}
  virtual ~Value () {}

  const std::shared_ptr<location>& loc () const { return loc_; }

  operator bool () const { return !is_nil(); }
  bool operator! () const { return is_nil(); }

  virtual bool is_nil () const { return false; }
  virtual bool is_variable () const { return false; }

  virtual double as_number () const { return NAN; }
  virtual std::shared_ptr<std::string> as_symbol () const { return nullptr; }
  virtual std::shared_ptr<Pair> as_pair (const std::shared_ptr<Value>& self) const { return nullptr; }

  void require_nil () const;
  double require_number (const location& loc) const;
  virtual std::string require_string (const location& loc) const;
  std::shared_ptr<std::string> require_symbol (const location& loc) const;
  std::shared_ptr<Pair> require_pair (const std::shared_ptr<Value>& self) const;
  virtual std::default_random_engine& require_random_state (const location& loc);

  virtual bool equals (const std::shared_ptr<Value>& other) const { return this == other.get(); }
  virtual bool equals_true () const { return false; }
  virtual bool equals_number (double value) const { return false; }
  virtual bool equals_string (const std::string& value) const { return false; }
  virtual bool equals_symbol (const std::shared_ptr<std::string>& value) const { return false; }
  virtual bool equals_pair (const std::shared_ptr<Value>& left, const std::shared_ptr<Value>& right) const { return false; }

  virtual std::string to_string () const = 0;
  virtual std::string to_rest_string () const { return " . " + to_string(); }

  virtual std::shared_ptr<Value> compile (Interpreter& interpreter, const std::shared_ptr<Value>& self) const { return self; }
  virtual std::shared_ptr<Value> compile_rest (Interpreter& interpreter, const std::shared_ptr<Value>& self) const;
  virtual std::shared_ptr<Value> compile_commas (Interpreter& interpreter, const std::shared_ptr<Value>& self) const;

  virtual std::shared_ptr<Value> expand (Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) const;

  virtual std::shared_ptr<Value> evaluate (Interpreter& interpreter, const std::shared_ptr<Value>& self) const { return self; }
  virtual std::shared_ptr<Value> evaluate_rest (Interpreter& interpreter, const std::shared_ptr<Value>& self) const;
  virtual std::shared_ptr<Value> evaluate_commas (Interpreter& interpreter, const std::shared_ptr<Value>& self) const;

  virtual std::shared_ptr<Value> invoke (Interpreter& interpreter, const Pair& pair) const;
  virtual std::shared_ptr<Value> invoke_macro (Interpreter& interpreter, const Pair& pair) const;

  virtual void set_value (Interpreter& interpreter, const std::shared_ptr<Value>& value, const location& loc) const;

private:

  std::shared_ptr<location> loc_;
};

class True : public Value {
public:

  explicit True (const std::shared_ptr<location>& loc = nullptr) : Value(loc) {}

  bool equals (const std::shared_ptr<Value>& other) const override { return other->equals_true(); }
  bool equals_true () const override { return true; }

  std::string to_string () const override { return "t"; }
};

class Nil : public Value {
public:

  explicit Nil (const std::shared_ptr<location>& loc = nullptr) : Value(loc) {}

  bool is_nil () const override { return true; }

  bool equals (const std::shared_ptr<Value>& other) const override { return other->is_nil(); }

  std::string to_string () const override { return "nil"; }
  std::string to_rest_string () const override { return ""; }
};

class Number : public Value {
public:

  explicit Number (double value, const std::shared_ptr<location>& loc = nullptr) : Value(loc), value_(value) {}

  double as_number () const override { return value_; }

  bool equals (const std::shared_ptr<Value>& other) const override { return other->equals_number(value_); }
  bool equals_number (double value) const override { return value == value_; }

  std::string to_string () const override;

private:

  double value_;
};

class String : public Value {
public:

  explicit String (const std::string& value, const std::shared_ptr<location>& loc = nullptr) : Value(loc), value_(value) {}

  std::string require_string (const location& loc) const override { return value_; }

  bool equals (const std::shared_ptr<Value>& other) const override { return other->equals_string(value_); }
  bool equals_string (const std::string& value) const override { return value == value_; }

  std::string to_string () const override;

private:

  std::string value_;
};

class Symbol : public Value {
public:

  explicit Symbol (const std::shared_ptr<std::string>& value, const std::shared_ptr<location>& loc = nullptr)
    : Value(loc), value_(value) {}

  std::shared_ptr<std::string> as_symbol () const override { return value_; }

  bool equals (const std::shared_ptr<Value>& other) const override { return other->equals_symbol(value_); }
  bool equals_symbol (const std::shared_ptr<std::string>& value) const override { return value == value_; }

  std::string to_string () const override { return *value_; }

  std::shared_ptr<Value> compile (Interpreter& interpreter, const std::shared_ptr<Value>& self) const override;

private:

  std::shared_ptr<std::string> value_;
};

class Pair : public Value {
public:

  Pair (const std::shared_ptr<Value>& left, const std::shared_ptr<Value>& right, const std::shared_ptr<location>& loc = nullptr)
    : Value(loc), left_(left), right_(right) {}

  const std::shared_ptr<Value>& left () const { return left_; }
  const std::shared_ptr<Value>& right () const { return right_; }

  void set_left (const std::shared_ptr<Value>& left) { left_ = left; }
  void set_right (const std::shared_ptr<Value>& right) { right_ = right; }

  std::shared_ptr<Pair> as_pair (const std::shared_ptr<Value>& self) const { return std::static_pointer_cast<Pair>(self); }

  bool equals (const std::shared_ptr<Value>& other) const override { return other->equals_pair(left_, right_); }
  bool equals_pair (const std::shared_ptr<Value>& left, const std::shared_ptr<Value>& right) const override {
    return left->equals(left_) && right->equals(right_);
  }

  std::string to_string () const override { return '(' + left_->to_string() + right_->to_rest_string() + ')'; }
  std::string to_rest_string () const override { return ' ' + left_->to_string() + right_->to_rest_string(); }

  std::shared_ptr<Value> compile (Interpreter& interpreter, const std::shared_ptr<Value>& self) const override;
  std::shared_ptr<Value> compile_rest (Interpreter& interpreter, const std::shared_ptr<Value>& self) const override;
  std::shared_ptr<Value> compile_commas (Interpreter& interpreter, const std::shared_ptr<Value>& self) const override;

  std::shared_ptr<Value> evaluate (Interpreter& interpreter, const std::shared_ptr<Value>& self) const override;
  std::shared_ptr<Value> evaluate_rest (Interpreter& interpreter, const std::shared_ptr<Value>& self) const override;
  std::shared_ptr<Value> evaluate_commas (Interpreter& interpreter, const std::shared_ptr<Value>& self) const override;

private:

  std::shared_ptr<Value> left_;
  std::shared_ptr<Value> right_;
};

class NamedValue : public Value {
public:

  NamedValue (const std::string& name) : name_(name) {}

  std::string to_string () const override { return name_; }

private:

  std::string name_;
};

template<typename Expand>
class Expander : public NamedValue {
public:

  Expander (const std::string& name, const Expand& expand) : NamedValue(name), expand_(expand) {}

  std::shared_ptr<Value> expand (
      Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) const override {
    return expand_(interpreter, pair, self);
  }

private:

  Expand expand_;
};

template<typename Invoke>
class Operator : public NamedValue {
public:

  Operator (const std::string& name, const Invoke& invoke) : NamedValue(name), invoke_(invoke) {}

  std::shared_ptr<Value> invoke (Interpreter& interpreter, const Pair& pair) const override {
    return invoke_(interpreter, pair.right());
  }

private:

  Invoke invoke_;
};

template<typename Expand, typename Invoke>
class ExpandOperator : public Expander<Expand> {
public:

  ExpandOperator (const std::string& name, const Expand& expand, const Invoke& invoke)
    : Expander<Expand>(name, expand), invoke_(invoke) {}

  std::shared_ptr<Value> invoke (Interpreter& interpreter, const Pair& pair) const override {
    return invoke_(interpreter, pair.right());
  }

private:

  Invoke invoke_;
};

template<typename Invoke>
class NativeFunction : public NamedValue {
public:

  NativeFunction (const std::string& name, const Invoke& invoke) : NamedValue(name), invoke_(invoke) {}

  std::shared_ptr<Value> invoke (Interpreter& interpreter, const Pair& pair) const override {
    return invoke_(interpreter, pair.right()->evaluate_rest(interpreter, pair.right()));
  }

private:

  Invoke invoke_;
};

class LambdaDefinition : public NamedValue {
public:

  LambdaDefinition (const std::string& name, Interpreter& interpreter, const std::shared_ptr<Value>& args);

  std::shared_ptr<Value> evaluate (Interpreter& interpreter, const std::shared_ptr<Value>& self) const override;

private:

  friend class LambdaFunction;

  struct optional_parameter {
    std::shared_ptr<std::string> var;
    std::shared_ptr<Value> initform;
    std::shared_ptr<std::string> svar;
  };

  struct aux_parameter {
    std::shared_ptr<std::string> var;
    std::shared_ptr<Value> initform;
  };

  std::shared_ptr<Value> invoke_lambda (
    Interpreter& interpreter, const std::shared_ptr<Invocation>& parent_context, const std::shared_ptr<Value>& args) const;

  std::vector<std::shared_ptr<std::string>> required_params_;
  std::vector<optional_parameter> optional_params_;
  std::shared_ptr<std::string> rest_param_;
  std::unordered_map<std::shared_ptr<std::string>, optional_parameter> key_params_;
  std::vector<aux_parameter> aux_params_;

  std::shared_ptr<Value> body_;
};

class LambdaFunction : public Value {
public:

  LambdaFunction (const std::shared_ptr<LambdaDefinition>& definition, const std::shared_ptr<Invocation>& parent_context)
    : definition_(definition), parent_context_(parent_context) {}

  std::string to_string () const override { return definition_->to_string(); }

  std::shared_ptr<Value> invoke (Interpreter& interpreter, const Pair& pair) const override;
  std::shared_ptr<Value> invoke_macro (Interpreter& interpreter, const Pair& pair) const override;

private:

  std::shared_ptr<LambdaDefinition> definition_;
  std::shared_ptr<Invocation> parent_context_;
};

class Variable : public Value {
public:

  Variable (const std::shared_ptr<std::string>& symbol_value, const std::shared_ptr<location>& loc)
    : Value(loc), symbol_value_(symbol_value) {}

  const std::shared_ptr<std::string>& symbol_value () const { return symbol_value_; }

  bool is_variable () const override { return true; }

  std::string to_string () const override { return *symbol_value_; }

private:

  std::shared_ptr<std::string> symbol_value_;
};

class LexicalVariable : public Variable {
public:

  explicit LexicalVariable (const std::shared_ptr<std::string>& symbol_value, const std::shared_ptr<location>& loc = nullptr)
    : Variable(symbol_value, loc) {}

  std::shared_ptr<Value> evaluate (Interpreter& interpreter, const std::shared_ptr<Value>& self) const override;

  void set_value (Interpreter& interpreter, const std::shared_ptr<Value>& value, const location& loc) const override;
};

class DynamicVariable : public Variable {
public:

  explicit DynamicVariable (const std::shared_ptr<std::string>& symbol_value, const std::shared_ptr<location>& loc = nullptr)
    : Variable(symbol_value, loc) {}

  std::shared_ptr<Value> evaluate (Interpreter& interpreter, const std::shared_ptr<Value>& self) const override;

  void set_value (Interpreter& interpreter, const std::shared_ptr<Value>& value, const location& loc) const override;
};

class Macro : public Value {
public:

  Macro (const std::shared_ptr<Value>& function) : function_(function) {}

  std::string to_string () const override { return function_->to_string(); }

  std::shared_ptr<Value> expand (Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) const override;

private:

  std::shared_ptr<Value> function_;
};

class RandomState : public NamedValue {
public:

  RandomState (const std::string& name, const std::default_random_engine& engine) : NamedValue(name), engine_(engine) {}

  std::default_random_engine& require_random_state (const location& loc) override { return engine_; }

private:

  std::default_random_engine engine_;
};

class Invocation {
public:

  Invocation (const std::shared_ptr<Invocation>& parent = nullptr) : parent_(parent) {}

  bool is_defined (const std::shared_ptr<std::string>& symbol_value) const { return values_.count(symbol_value) > 0; }
  void define (const std::shared_ptr<std::string>& symbol_value, const std::shared_ptr<Value>& value);

  std::shared_ptr<Value> lookup_lexical_value (const std::shared_ptr<std::string>& symbol_value) const;
  void set_lexical_value (const std::shared_ptr<std::string>& symbol_value, const std::shared_ptr<Value>& value);

  std::shared_ptr<Value> lookup_dynamic_value (const std::shared_ptr<std::string>& symbol_value) const;
  bool set_dynamic_value (const std::shared_ptr<std::string>& symbol_value, const std::shared_ptr<Value>& value);

private:

  std::shared_ptr<Invocation> parent_;
  std::unordered_map<std::shared_ptr<std::string>, std::shared_ptr<Value>> values_;
};

struct location {
  std::shared_ptr<std::string> filename;
  int line;
  int column;

  std::string to_string () const { return *filename + ' ' + std::to_string(line) + ':' + std::to_string(column); }
};

struct lexeme {
  char character;
  std::shared_ptr<Value> value;
  location loc;
};

class script_error : public std::exception {
public:

  script_error (const std::string& what, const location& loc) : what_(what + " at " + loc.to_string()) {}

  const char* what () const noexcept override { return what_.c_str(); }

private:

  std::string what_;
};

}

#endif // GOLDBERG_H
