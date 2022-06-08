#include <cctype>
#include <fstream>
#include <sstream>

#include "goldberg.hpp"

namespace goldberg {

Interpreter::Interpreter () : call_stack_({{std::make_shared<invocation>()}}) {
  register_builtins();
}

std::shared_ptr<Value> Interpreter::evaluate (const std::string& string, const std::string& filename) {
  std::istringstream in(string);
  return load(in, filename);
}

std::shared_ptr<Value> Interpreter::load (const std::string& filename) {
  std::ifstream in(filename);
  return load(in, filename);
}

std::shared_ptr<Value> Interpreter::load (std::istream& in, const std::string& filename) {
  location loc{std::make_shared<std::string>(filename), 1, 1};
  auto last_result = std::shared_ptr<Value>(new Nil(std::make_shared<location>(loc)));
  while (in) {
    auto result = evaluate(parse(in, loc));
    if (*result) last_result = result;
  }
  return last_result;
}

void Interpreter::register_builtin (const std::string& name, const std::shared_ptr<Value>& value) {
  call_stack_.front().context->lexical_vars[get_symbol_value(name)] = value;
}

std::shared_ptr<Value> Interpreter::parse (std::istream& in, location& loc) {
  return parse(in, loc, lex(in, loc));
}

std::shared_ptr<Value> Interpreter::parse (std::istream& in, location& loc, const lexeme& token) {
  switch (token.character) {
    case '.':
      throw script_error("Unexpected '.'", token.loc);

    case '(':
      return parse_rest(in, loc);

    case ')':
      throw script_error("Mismatched ')'", token.loc);

    case '\'': {
      auto token_loc = std::make_shared<location>(token.loc);
      return std::shared_ptr<Value>(new Pair(
        std::shared_ptr<Value>(new Symbol(get_symbol_value("quote"), token_loc)),
        std::shared_ptr<Value>(new Pair(parse(in, loc), std::shared_ptr<Value>(new Nil(token_loc)), token_loc)),
        token_loc));
    }
    default:
      return token.value;
  }
}

std::shared_ptr<Value> Interpreter::parse_rest (std::istream& in, location& loc) {
  auto token = lex(in, loc);
  switch (token.character) {
    case ')':
      return std::shared_ptr<Value>(new Nil(std::make_shared<location>(token.loc)));

    case '.':
      return parse(in, loc);

    default: {
      auto first = parse(in, loc, token);
      return std::shared_ptr<Value>(new Pair(first, parse_rest(in, loc), first->loc()));
    }
  }
}

lexeme Interpreter::lex (std::istream& in, location& loc) {
  while (in.good()) {
    int ch = in.get();
    switch (ch) {
      case std::char_traits<char>::eof():
        break;

      case ';':
        while (in.good() && in.get() != '\n'); // consume rest of line
      case '\n':
        loc.line++;
        loc.column = 1;
        break;

      case '"': {
        auto start = loc;
        loc.column++;
        std::string value;
        while (in.good()) {
          ch = in.get();
          switch (ch) {
            case std::char_traits<char>::eof():
              break;

            case '\n':
              loc.line++;
              loc.column = 1;
              value += '\n';
              break;

            case '\\':
              loc.column++;
              ch = in.get();
              switch (ch) {
                case std::char_traits<char>::eof():
                  break;

                case 'n':
                  value += '\n';
                  loc.column++;
                  break;

                case '"':
                  value += '"';
                  loc.column++;
                  break;

                case '\\':
                  value += '\\';
                  loc.column++;
                  break;

                default:
                  if (!std::isspace(ch)) {
                    throw script_error("Unrecognized escape code '" + static_cast<char>(ch) + '\'', loc);
                  }
                  in.unget();
                  while (in.good()) { // consume whitespace
                    ch = in.get();
                    switch (ch) {
                      case std::char_traits<char>::eof():
                        throw script_error("Unmatched '\"'", start);

                      case '\n':
                        loc.line++;
                        loc.column = 1;
                        break;

                      default:
                        if (!std::isspace(ch)) {
                          in.unget();
                          goto whitespace_ended;
                        }
                        loc.column++;
                        break;
                    }
                  }
                  whitespace_ended:
                  break;
              }
              break;

            case '"': {
              lexeme token{0, std::shared_ptr<Value>(new String(value, std::make_shared<location>(start))), start};
              loc.column++;
              return token;
            }
            default:
              value += static_cast<char>(ch);
              loc.column++;
              break;
          }
        }
        throw script_error("Unmatched '\"'", start);
      }
      case '(':
      case ')':
      case '\'': {
        lexeme token{static_cast<char>(ch), nullptr, loc};
        loc.column++;
        return token;
      }
      case '.': {
        int next = in.get();
        in.unget();
        if (!std::isdigit(next)) { // if the next character is a digit, fall through to lex as number
          lexeme token{'.', nullptr, loc};
          loc.column++;
          return token;
        }
      }
      default: {
        if (std::isspace(ch)) {
          loc.column++;
          break;
        }
        auto start = loc;
        loc.column++;
        std::string value(1, static_cast<char>(ch));
        while (in.good()) {
          ch = in.get();
          switch (ch) {
            case std::char_traits<char>::eof():
              goto token_ended;

            case ')':
              in.unget();
              goto token_ended;

            default:
              if (std::isspace(ch)) {
                in.unget();
                goto token_ended;
              }
              value += static_cast<char>(ch);
              loc.column++;
              break;
          }
        }
        token_ended:
        char first = value[0];
        char second = value.length() > 1 ? value[1] : 0;
        return first == '.' || std::isdigit(first) || (first == '+' || first == '-') && (second == '.' || std::isdigit(second))
          ? lexeme{0, std::shared_ptr<Value>(new Number(std::stod(value), std::make_shared<location>(start))), start}
          : lexeme{0, std::shared_ptr<Value>(new Symbol(get_symbol_value(value), std::make_shared<location>(start))), start};
      }
    }
  }
  return {0, std::shared_ptr<Value>(new Nil(std::make_shared<location>(loc))), loc};
}

std::shared_ptr<Value> Interpreter::evaluate (const std::shared_ptr<Value>& value) {
  return value->evaluate(*this, value);
}

std::shared_ptr<std::string> Interpreter::get_symbol_value (const std::string& value) {
  auto& symbol_value = symbol_values_[value];
  if (!symbol_value.expired()) return std::shared_ptr<std::string>(symbol_value);
  auto new_symbol_value = std::make_shared<std::string>(value);
  symbol_value = new_symbol_value;
  return new_symbol_value;
}

std::shared_ptr<Value> Interpreter::lookup (const std::shared_ptr<std::string>& symbol_value) const {
  auto it = call_stack_.rbegin();
  auto lexical_value = it->context->lookup(symbol_value);
  if (lexical_value) return lexical_value;

  for (; it != call_stack_.rend(); ++it) {
    auto value = it->dynamic_vars.find(symbol_value);
    if (value != it->dynamic_vars.end()) return value->second;
  }
  return std::shared_ptr<Value>();
}

void Value::require_nil () const {
  if (!is_nil()) throw script_error("Unexpected argument", *loc());
}

double Value::require_number (const location& loc) const {
  throw script_error("Expected number", loc);
}

std::shared_ptr<Pair> Value::require_pair (const std::shared_ptr<Value>& self) const {
  throw script_error("Expected argument", *loc());
}

std::shared_ptr<Value> Value::evaluate_rest (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  return evaluate(interpreter, self);
}

std::shared_ptr<Value> Value::invoke (
    Interpreter& interpreter, const std::shared_ptr<Value>& args, const std::shared_ptr<Value>& source) const {
  throw script_error("Cannot invoke value " + source->to_string(), *source->loc());
}

std::string Number::to_string () const {
  std::ostringstream out;
  out << value_;
  return out.str();
}

std::string String::to_string () const {
  std::string string(1, '"');
  for (char ch : value_) {
    switch (ch) {
      case '\n':
        string += "\\n";
        break;

      case '\\':
      case '"':
        string += '\\'; // fall through

      default:
        string += ch;
        break;
    }
  }
  return string += '"';
}

std::shared_ptr<Value> Symbol::evaluate (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  auto result = interpreter.lookup(value_);
  if (result) return result;
  throw script_error("Unknown symbol \"" + *value_ + '"', *loc());
}

std::shared_ptr<Pair> Pair::require_pair (const std::shared_ptr<Value>& self) const {
  return std::static_pointer_cast<Pair>(self);
}

std::shared_ptr<Value> Pair::evaluate (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  auto fn = left_->evaluate(interpreter, left_);
  return fn->invoke(interpreter, right_, left_);
}

std::shared_ptr<Value> Pair::evaluate_rest (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  auto left = left_->evaluate(interpreter, left_);
  auto right = right_->evaluate_rest(interpreter, right_);
  return std::shared_ptr<Value>(new Pair(left, right, loc()));
}

std::shared_ptr<Value> invocation::lookup (const std::shared_ptr<std::string>& symbol_value) const {
  auto value = lexical_vars.find(symbol_value);
  if (value != lexical_vars.end()) return value->second;
  return parent ? parent->lookup(symbol_value) : std::shared_ptr<Value>();
}

}
