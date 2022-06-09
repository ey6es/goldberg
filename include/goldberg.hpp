#ifndef GOLDBERG_H
#define GOLDBERG_H

#include <exception>
#include <istream>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace goldberg {

class Invocation;
class Value;
struct lexeme;
struct location;
struct stack_frame;

class Interpreter {
public:

  static const std::shared_ptr<Value>& t () { return t_; }
  static const std::shared_ptr<Value>& nil () { return nil_; }

  static std::shared_ptr<std::string> static_symbol_value (const std::string& value);

  Interpreter ();

  const std::shared_ptr<Invocation>& current_context () const;

  std::shared_ptr<Value> evaluate (const std::string& string, const std::string& filename = "<string>");

  std::shared_ptr<Value> load (const std::string& filename);
  std::shared_ptr<Value> load (std::istream& in, const std::string& filename);

private:

  friend class Symbol;
  friend class Lambda;

  static std::shared_ptr<Value> t_;
  static std::shared_ptr<Value> nil_;

  static std::unordered_map<std::string, std::shared_ptr<std::string>> static_symbol_values_;

  static std::shared_ptr<Invocation> create_builtin_context ();

  std::shared_ptr<Value> parse (std::istream& in, location& loc);
  std::shared_ptr<Value> parse (std::istream& in, location& loc, const lexeme& token);
  std::shared_ptr<Value> parse_rest (std::istream& in, location& loc);

  lexeme lex (std::istream& in, location& loc);

  std::shared_ptr<Value> evaluate (const std::shared_ptr<Value>& value);

  std::shared_ptr<std::string> get_symbol_value (const std::string& value);

  std::shared_ptr<Value> lookup (const std::shared_ptr<std::string>& symbol_value) const;

  void push_frame (const std::shared_ptr<Invocation>& parent_context);
  void pop_frame () { call_stack_.pop_back(); }

  std::unordered_map<std::string, std::weak_ptr<std::string>> symbol_values_;

  std::vector<stack_frame> call_stack_;
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

  void require_nil () const;
  virtual double require_number (const location& loc) const;
  virtual std::shared_ptr<Pair> require_pair (const std::shared_ptr<Value>& self) const;

  virtual bool equals (const std::shared_ptr<Value>& other) const { return this == other.get(); }
  virtual bool equals_true () const { return false; }
  virtual bool equals_number (double value) const { return false; }
  virtual bool equals_string (const std::string& value) const { return false; }
  virtual bool equals_symbol (const std::shared_ptr<std::string>& value) const { return false; }
  virtual bool equals_pair (const std::shared_ptr<Value>& left, const std::shared_ptr<Value>& right) const { return false; }

  virtual std::string to_string () const = 0;
  virtual std::string to_rest_string () const { return " . " + to_string(); }

  virtual std::shared_ptr<Value> evaluate (Interpreter& interpreter, const std::shared_ptr<Value>& self) const { return self; }
  virtual std::shared_ptr<Value> evaluate_rest (Interpreter& interpreter, const std::shared_ptr<Value>& self) const;

  virtual std::shared_ptr<Value> invoke (
    Interpreter& interpreter, const std::shared_ptr<Value>& args, const location& loc) const;

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

  double require_number (const location& loc) const override { return value_; }

  bool equals (const std::shared_ptr<Value>& other) const override { return other->equals_number(value_); }
  bool equals_number (double value) const override { return value == value_; }

  std::string to_string () const override;

private:

  double value_;
};

class String : public Value {
public:

  explicit String (const std::string& value, const std::shared_ptr<location>& loc = nullptr) : Value(loc), value_(value) {}

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

  bool equals (const std::shared_ptr<Value>& other) const override { return other->equals_symbol(value_); }
  bool equals_symbol (const std::shared_ptr<std::string>& value) const override { return value == value_; }

  std::string to_string () const override { return *value_; }

  std::shared_ptr<Value> evaluate (Interpreter& interpreter, const std::shared_ptr<Value>& self) const override;

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

  std::shared_ptr<Pair> require_pair (const std::shared_ptr<Value>& self) const override;

  bool equals (const std::shared_ptr<Value>& other) const override { return other->equals_pair(left_, right_); }
  bool equals_pair (const std::shared_ptr<Value>& left, const std::shared_ptr<Value>& right) const override {
    return left->equals(left_) && right->equals(right_);
  }

  std::string to_string () const override { return '(' + left_->to_string() + right_->to_rest_string() + ')'; }
  std::string to_rest_string () const override { return ' ' + left_->to_string() + right_->to_rest_string(); }

  std::shared_ptr<Value> evaluate (Interpreter& interpreter, const std::shared_ptr<Value>& self) const override;
  std::shared_ptr<Value> evaluate_rest (Interpreter& interpreter, const std::shared_ptr<Value>& self) const override;

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

template<typename T>
class NativeOperator : public NamedValue {
public:

  NativeOperator (const std::string& name, const T& function) : NamedValue(name), function_(function) {}

  std::shared_ptr<Value> invoke (
      Interpreter& interpreter, const std::shared_ptr<Value>& args, const location& loc) const override {
    return function_(interpreter, args);
  }

private:

  T function_;
};

template<typename T>
class NativeFunction : public NamedValue {
public:

  NativeFunction (const std::string& name, const T& function) : NamedValue(name), function_(function) {}

  std::shared_ptr<Value> invoke (
      Interpreter& interpreter, const std::shared_ptr<Value>& args, const location& loc) const override {
    return function_(interpreter, args->evaluate_rest(interpreter, args));
  }

private:

  T function_;
};

class Lambda : public NamedValue {
public:

  Lambda (const std::string& name, const std::shared_ptr<Invocation>& context, const std::shared_ptr<Value>& definition)
    : NamedValue(name), context_(context), definition_(definition) {}

  std::shared_ptr<Value> invoke (
      Interpreter& interpreter, const std::shared_ptr<Value>& args, const location& loc) const override;

private:

  std::shared_ptr<Invocation> context_;
  std::shared_ptr<Value> definition_;
};

class Invocation {
public:

  Invocation (const std::shared_ptr<Invocation>& parent = nullptr) : parent_(parent) {}

  void define (const std::shared_ptr<std::string>& symbol_value, const std::shared_ptr<Value>& value);

  std::shared_ptr<Value> lookup (const std::shared_ptr<std::string>& symbol_value) const;

private:

  std::shared_ptr<Invocation> parent_;
  std::unordered_map<std::shared_ptr<std::string>, std::shared_ptr<Value>> values_;
};

struct stack_frame {
  std::shared_ptr<Invocation> context;
  std::unordered_map<std::shared_ptr<std::string>, std::shared_ptr<Value>> dynamic_vars;
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
