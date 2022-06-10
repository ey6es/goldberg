#include <cctype>
#include <fstream>
#include <sstream>

#include "goldberg.hpp"

namespace goldberg {

std::shared_ptr<Value> Interpreter::t_ = std::make_shared<True>();
std::shared_ptr<Value> Interpreter::nil_ = std::make_shared<Nil>();

std::unordered_map<std::string, std::shared_ptr<std::string>> Interpreter::static_symbol_values_;

std::shared_ptr<std::string> Interpreter::static_symbol_value (const std::string& value) {
  auto& pointer = static_symbol_values_[value];
  if (!pointer) pointer = std::make_shared<std::string>(value);
  return pointer;
}

Interpreter::Interpreter () {
  static auto builtin_context = create_builtin_context();
  push_frame(std::make_shared<Invocation>(builtin_context));
}

const std::shared_ptr<Invocation>& Interpreter::current_context () const {
  return call_stack_.back().context;
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
  std::shared_ptr<Value> last_result = std::make_shared<Nil>(std::make_shared<location>(loc));
  while (in) {
    auto result = evaluate(parse(in, loc));
    if (*result) last_result = result;
  }
  return last_result;
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
      return std::make_shared<Pair>(
        std::make_shared<Symbol>(get_symbol_value("quote"), token_loc),
        std::make_shared<Pair>(parse(in, loc), std::make_shared<Nil>(token_loc), token_loc),
        token_loc);
    }
    default:
      return token.value;
  }
}

std::shared_ptr<Value> Interpreter::parse_rest (std::istream& in, location& loc) {
  auto token = lex(in, loc);
  switch (token.character) {
    case ')':
      return std::make_shared<Nil>(std::make_shared<location>(token.loc));

    case '.':
      return parse(in, loc);

    default: {
      auto first = parse(in, loc, token);
      return std::make_shared<Pair>(first, parse_rest(in, loc), first->loc());
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
              lexeme token{0, std::make_shared<String>(value, std::make_shared<location>(start)), start};
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
          ? lexeme{0, std::make_shared<Number>(std::stod(value), std::make_shared<location>(start)), start}
          : lexeme{0, std::make_shared<Symbol>(get_symbol_value(value), std::make_shared<location>(start)), start};
      }
    }
  }
  return {0, std::make_shared<Nil>(std::make_shared<location>(loc)), loc};
}

std::shared_ptr<Value> Interpreter::evaluate (const std::shared_ptr<Value>& value) {
  return value->evaluate(*this, value);
}

std::shared_ptr<std::string> Interpreter::get_symbol_value (const std::string& value) {
  auto it = static_symbol_values_.find(value);
  if (it != static_symbol_values_.end()) return it->second;

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

void Interpreter::push_frame (const std::shared_ptr<Invocation>& context) {
  call_stack_.push_back({context});
}

void Value::require_nil () const {
  if (!is_nil()) throw script_error("Unexpected argument", *loc());
}

double Value::require_number (const location& loc) const {
  throw script_error("Expected number", loc);
}

std::shared_ptr<std::string> Value::require_symbol (const location& loc) const {
  auto result = as_symbol();
  if (result) return result;
  throw script_error("Expected symbol", loc);
}

std::shared_ptr<Pair> Value::require_pair (const std::shared_ptr<Value>& self) const {
  auto result = as_pair(self);
  if (result) return result;
  throw script_error("Expected argument", *loc());
}

std::shared_ptr<Value> Value::evaluate_rest (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  return evaluate(interpreter, self);
}

std::shared_ptr<Value> Value::invoke (Interpreter& interpreter, const std::shared_ptr<Value>& args, const location& loc) const {
  throw script_error("Expected function", loc);
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
  if (value_->size() > 0 && value_->front() == ':') return self;
  auto result = interpreter.lookup(value_);
  if (result) return result;
  throw script_error("Unknown symbol \"" + *value_ + '"', *loc());
}

std::shared_ptr<Value> Pair::evaluate (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  auto fn = left_->evaluate(interpreter, left_);
  return fn->invoke(interpreter, right_, *loc());
}

std::shared_ptr<Value> Pair::evaluate_rest (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  auto left = left_->evaluate(interpreter, left_);
  auto right = right_->evaluate_rest(interpreter, right_);
  return std::make_shared<Pair>(left, right, loc());
}

static auto aux_keyword = Interpreter::static_symbol_value("&aux");
static auto key_keyword = Interpreter::static_symbol_value("&key");
static auto allow_other_keys_keyword = Interpreter::static_symbol_value("&allow-other-keys");
static auto rest_keyword = Interpreter::static_symbol_value("&rest");
static auto optional_keyword = Interpreter::static_symbol_value("&optional");

parameters::parameters (Interpreter& interpreter, const std::shared_ptr<Value>& spec) : allow_other_keys_(false) {
  auto next_param = spec;

  auto is_keyword = [](const auto& symbol_value) {
    return symbol_value->length() > 0 && symbol_value->front() == '&';
  };

  auto process_aux = [&](const auto& keyword_value, const auto& keyword_loc) {
    if (keyword_value != aux_keyword) {
      throw script_error("Unknown keyword \"" + *keyword_value + '"', keyword_loc);
    }
    while (*next_param) {
      auto next_param_pair = next_param->require_pair(next_param);
      auto binding_pair = next_param_pair->left()->as_pair(next_param_pair->left());
      next_param = next_param_pair->right();
      std::shared_ptr<std::string> symbol_value;
      std::shared_ptr<Value> initform = Interpreter::nil();
      if (binding_pair) {
        symbol_value = binding_pair->left()->require_symbol(*binding_pair->loc());
        if (*binding_pair->right()) {
          auto initform_pair = binding_pair->right()->require_pair(binding_pair->right());
          initform = initform_pair->left();
          initform_pair->right()->require_nil();
        }
      } else {
        symbol_value = next_param_pair->left()->require_symbol(*next_param_pair->loc());
      }
      aux_.push_back({symbol_value, initform});
    }
  };

  auto process_key = [&](const auto& keyword_value, const auto& keyword_loc) {
    if (keyword_value != key_keyword) {
      process_aux(keyword_value, keyword_loc);
      return;
    }
    while (*next_param) {
      auto next_param_pair = next_param->require_pair(next_param);
      auto binding_pair = next_param_pair->left()->as_pair(next_param_pair->left());
      next_param = next_param_pair->right();
      std::shared_ptr<std::string> symbol_value;
      std::shared_ptr<std::string> keyword_symbol;
      std::shared_ptr<Value> initform = Interpreter::nil();
      std::shared_ptr<std::string> svar;
      if (binding_pair) {
        auto keyword_pair = binding_pair->left()->as_pair(binding_pair->left());
        if (keyword_pair) {
          keyword_symbol = keyword_pair->left()->require_symbol(*keyword_pair->loc());
          auto value_pair = keyword_pair->right()->require_pair(keyword_pair->right());
          symbol_value = value_pair->left()->require_symbol(*value_pair->loc());
          value_pair->right()->require_nil();

        } else {
          symbol_value = binding_pair->left()->require_symbol(*binding_pair->loc());
          keyword_symbol = interpreter.get_symbol_value(':' + *symbol_value);
        }
        if (*binding_pair->right()) {
          auto initform_pair = binding_pair->right()->require_pair(binding_pair->right());
          initform = initform_pair->left();
          if (*initform_pair->right()) {
            auto svar_pair = initform_pair->right()->require_pair(initform_pair->right());
            svar = svar_pair->left()->require_symbol(*svar_pair->loc());
            svar_pair->right()->require_nil();
          }
        }
      } else {
        auto loc = next_param_pair->loc();
        symbol_value = next_param_pair->left()->require_symbol(*loc);
        keyword_symbol = interpreter.get_symbol_value(':' + *symbol_value);
        if (is_keyword(symbol_value)) {
          if (symbol_value == allow_other_keys_keyword) {
            allow_other_keys_ = true;
            if (!*next_param) break;
            next_param_pair = next_param->require_pair(next_param);
            loc = next_param_pair->loc();
            symbol_value = next_param_pair->left()->require_symbol(*loc);
          }
          process_aux(symbol_value, *loc);
          break;
        }
      }
      key_[keyword_symbol] = {symbol_value, initform, svar};
    }
  };

  auto process_rest = [&](const auto& keyword_value, const auto& keyword_loc) {
    if (keyword_value != rest_keyword) {
      process_key(keyword_value, keyword_loc);
      return;
    }
    auto next_param_pair = next_param->require_pair(next_param);
    rest_ = next_param_pair->left()->require_symbol(*next_param_pair->loc());
    next_param = next_param_pair->right();
    if (*next_param) {
      next_param_pair = next_param->require_pair(next_param);
      auto loc = next_param_pair->loc();
      auto symbol_value = next_param_pair->left()->require_symbol(*loc);
      next_param = next_param_pair->right();
      if (!is_keyword(symbol_value)) throw script_error("Unexpected argument \"" + *symbol_value + '"', *loc);
      process_key(symbol_value, *loc);
    }
  };

  auto process_optional = [&](const auto& keyword_value, const auto& keyword_loc) {
    if (keyword_value != optional_keyword) {
      process_rest(keyword_value, keyword_loc);
      return;
    }
    while (*next_param) {
      auto next_param_pair = next_param->require_pair(next_param);
      auto binding_pair = next_param_pair->left()->as_pair(next_param_pair->left());
      next_param = next_param_pair->right();
      std::shared_ptr<std::string> symbol_value;
      std::shared_ptr<Value> initform = Interpreter::nil();
      std::shared_ptr<std::string> svar;
      if (binding_pair) {
        symbol_value = binding_pair->left()->require_symbol(*binding_pair->loc());
        if (*binding_pair->right()) {
          auto initform_pair = binding_pair->right()->require_pair(binding_pair->right());
          initform = initform_pair->left();
          if (*initform_pair->right()) {
            auto svar_pair = initform_pair->right()->require_pair(initform_pair->right());
            svar = svar_pair->left()->require_symbol(*svar_pair->loc());
            svar_pair->right()->require_nil();
          }
        }
      } else {
        auto loc = next_param_pair->loc();
        symbol_value = next_param_pair->left()->require_symbol(*loc);
        if (is_keyword(symbol_value)) {
          process_rest(symbol_value, *loc);
          break;
        }
      }
      optional_.push_back({symbol_value, initform, svar});
    }
  };

  while (*next_param) {
    auto next_param_pair = next_param->require_pair(next_param);
    auto loc = next_param_pair->loc();
    auto symbol_value = next_param_pair->left()->require_symbol(*loc);
    next_param = next_param_pair->right();
    if (is_keyword(symbol_value)) {
      process_optional(symbol_value, *loc);
      break;
    }
    required_.push_back(symbol_value);
  }
}

void parameters::bind (
    Interpreter& interpreter, const std::shared_ptr<Value>& args, const std::shared_ptr<Invocation>& ctx) const {
  auto next_arg = args;

  for (auto& var : required_) {
    auto next_arg_pair = next_arg->require_pair(next_arg);
    ctx->define(var, next_arg_pair->left());
    next_arg = next_arg_pair->right();
  }
  for (auto& optional : optional_) {
    if (*next_arg) {
      auto next_arg_pair = next_arg->require_pair(next_arg);
      ctx->define(optional.var, next_arg_pair->left());
      if (optional.svar) ctx->define(optional.svar, Interpreter::t());
      next_arg = next_arg_pair->right();

    } else {
      ctx->define(optional.var, optional.initform->evaluate(interpreter, optional.initform));
      if (optional.svar) ctx->define(optional.svar, Interpreter::nil());
    }
  }
  auto filtered_rest = next_arg;
  if (!key_.empty()) {
    filtered_rest = Interpreter::nil();
    std::shared_ptr<Pair> last_filtered;

    while (*next_arg) {
      auto next_arg_pair = next_arg->require_pair(next_arg);
      auto symbol_value = next_arg_pair->left()->as_symbol();
      next_arg = next_arg_pair->right();
      if (symbol_value) {
        auto it = key_.find(symbol_value);
        if (it != key_.end()) {
          next_arg_pair = next_arg->require_pair(next_arg);
          ctx->define(it->second.var, next_arg_pair->left());
          if (it->second.svar) ctx->define(it->second.svar, Interpreter::t());
          next_arg = next_arg_pair->right();
          continue;

        } else if (allow_other_keys_) {
          next_arg_pair = next_arg->require_pair(next_arg);
          ctx->define(symbol_value, next_arg_pair->left());
          next_arg = next_arg_pair->right();
          continue;
        }
      }
      auto new_last = std::make_shared<Pair>(next_arg_pair->left(), Interpreter::nil());
      if (last_filtered) last_filtered->set_right(new_last);
      else filtered_rest = new_last;
      last_filtered = new_last;
    }

    for (auto& pair : key_) {
      if (!ctx->is_defined(pair.second.var)) {
        ctx->define(pair.second.var, pair.second.initform->evaluate(interpreter, pair.second.initform));
        ctx->define(pair.second.svar, Interpreter::nil());
      }
    }
  }
  if (rest_) ctx->define(rest_, filtered_rest);
  else filtered_rest->require_nil();

  for (auto& aux : aux_) {
    ctx->define(aux.var, aux.initform->evaluate(interpreter, aux.initform));
  }
}

std::shared_ptr<Value> Lambda::invoke (
    Interpreter& interpreter, const std::shared_ptr<Value>& args, const location& loc) const {
  auto ctx = std::make_shared<Invocation>(parent_context_);
  parameters_.bind(interpreter, args->evaluate_rest(interpreter, args), ctx);
  interpreter.push_frame(ctx);

  auto last_result = Interpreter::nil();
  auto next_statement = body_;
  while (*next_statement) {
    auto next_statement_pair = next_statement->require_pair(next_statement);
    last_result = next_statement_pair->left()->evaluate(interpreter, next_statement_pair->left());
    next_statement = next_statement_pair->right();
  }
  interpreter.pop_frame();

  return last_result;
}

void Invocation::define (const std::shared_ptr<std::string>& symbol_value, const std::shared_ptr<Value>& value) {
  values_[symbol_value] = value;
}

std::shared_ptr<Value> Invocation::lookup (const std::shared_ptr<std::string>& symbol_value) const {
  auto value = values_.find(symbol_value);
  if (value != values_.end()) return value->second;
  return parent_ ? parent_->lookup(symbol_value) : std::shared_ptr<Value>();
}

}
