#ifndef GOLDBERG_H
#define GOLDBERG_H

#include <exception>
#include <istream>
#include <memory>
#include <string>
#include <unordered_map>

namespace goldberg {

class Value {
public:

  virtual ~Value () {}

  operator bool () const { return !is_nil(); }
  bool operator! () const { return is_nil(); }

  virtual bool is_nil () const { return false; }

  virtual std::string to_string () const = 0;

  virtual std::string to_rest_string () const { return " . " + to_string(); }
};

class True : public Value {
public:

  std::string to_string () const override { return "t"; }
};

class Nil : public Value {
public:

  bool is_nil () const override { return true; }

  std::string to_string () const override { return "nil"; }

  std::string to_rest_string () const override { return ""; }
};

class Number : public Value {
public:

  Number (double value) : value_(value) {}

  std::string to_string () const override;

private:

  double value_;
};

class String : public Value {
public:

  String (const std::string& value) : value_(value) {}

  std::string to_string () const override { return '"' + value_ + '"'; }

private:

  std::string value_;
};

class Symbol : public Value {
public:

  Symbol (const std::string& value) : value_(value) {}

  std::string to_string () const override { return value_; }

private:

  std::string value_;
};

class Pair : public Value {
public:

  Pair (const std::shared_ptr<Value>& left, const std::shared_ptr<Value>& right) : left_(left), right_(right) {}

  std::string to_string () const override { return '(' + left_->to_string() + right_->to_rest_string() + ')'; }

  std::string to_rest_string () const override { return ' ' + left_->to_string() + right_->to_rest_string(); }

private:

  std::shared_ptr<Value> left_;
  std::shared_ptr<Value> right_;
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

class Interpreter {
public:

  std::shared_ptr<Value> evaluate (const std::string& string, const std::string& filename = "<string>");

  std::shared_ptr<Value> load (const std::string& filename);

  std::shared_ptr<Value> load (std::istream& in, const std::string& filename);

private:

  std::shared_ptr<Value> parse (std::istream& in, location& loc);
  std::shared_ptr<Value> parse (std::istream& in, location& loc, const lexeme& token);

  std::shared_ptr<Value> parse_rest (std::istream& in, location& loc);

  lexeme lex (std::istream& in, location& loc);

  std::shared_ptr<Value> evaluate (const std::shared_ptr<Value>& value);

  std::shared_ptr<Value> get_symbol (const std::string& value);

  std::unordered_map<std::string, std::weak_ptr<Value>> symbols_;
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
