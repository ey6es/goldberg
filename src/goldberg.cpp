#include <cctype>
#include <fstream>
#include <regex>
#include <sstream>

#include "goldberg.hpp"

namespace goldberg {

std::shared_ptr<Value> Interpreter::t_ = std::make_shared<True>();
std::shared_ptr<Value> Interpreter::nil_ = std::make_shared<Nil>();

std::unordered_map<std::string, std::shared_ptr<std::string>> Interpreter::static_symbol_values_;

static auto quote_symbol = Interpreter::static_symbol_value("quote");
static auto backquote_symbol = Interpreter::static_symbol_value("backquote");
static auto comma_symbol = Interpreter::static_symbol_value("comma");
static auto comma_at_symbol = Interpreter::static_symbol_value("comma-at");
static auto function_symbol = Interpreter::static_symbol_value("function");
static auto aux_keyword = Interpreter::static_symbol_value("&aux");
static auto key_keyword = Interpreter::static_symbol_value("&key");
static auto rest_keyword = Interpreter::static_symbol_value("&rest");
static auto optional_keyword = Interpreter::static_symbol_value("&optional");

bindings Interpreter::static_bindings_;
std::shared_ptr<Invocation> Interpreter::static_context_ = std::make_shared<Invocation>();
bool Interpreter::statics_populated_ = populate_statics();

std::shared_ptr<std::string> Interpreter::static_symbol_value (const std::string& name) {
  auto& pointer = static_symbol_values_[name];
  if (!pointer) pointer = std::make_shared<std::string>(name);
  return pointer;
}

Interpreter::Interpreter () {
  push_bindings({});
  push_frame(std::make_shared<Invocation>(static_context_));

  evaluate("(defvar *gensym-counter* 0)");
  evaluate("(defvar *random-state* (make-random-state t))");
}

std::shared_ptr<Value> Interpreter::require (const std::string& filename) {
  auto& value = required_files_[filename];
  if (!value) value = load(filename);
  return value;
}

std::shared_ptr<Value> Interpreter::load (const std::string& filename) {
  std::ifstream in(filename);
  return evaluate(in, filename);
}

std::shared_ptr<Value> Interpreter::parse (const std::string& string, const std::string& filename) {
  std::istringstream in(string);
  return parse(in, filename);
}

std::shared_ptr<Value> Interpreter::parse (std::istream& in, const std::string& filename) {
  location loc(filename);
  std::shared_ptr<Value> last_result = std::make_shared<Nil>(std::make_shared<location>(loc));
  while (in) {
    auto parsed = parse(in, loc);
    if (*parsed) last_result = parsed;
  }
  return last_result;
}

std::shared_ptr<Value> Interpreter::evaluate (const std::string& string, const std::string& filename) {
  std::istringstream in(string);
  return evaluate(in, filename);
}

std::shared_ptr<Value> Interpreter::evaluate (std::istream& in, const std::string& filename) {
  location loc(filename);
  std::shared_ptr<Value> last_result = std::make_shared<Nil>(std::make_shared<location>(loc));
  while (in) {
    auto result = evaluate(parse(in, loc));
    if (*result) last_result = result;
  }
  return last_result;
}

std::shared_ptr<Value> Interpreter::evaluate (const std::shared_ptr<Value> value) {
  struct global_scope {
    Interpreter& interpreter;
    std::vector<std::shared_ptr<Invocation>> saved_call_stack;

    global_scope (Interpreter& interpreter) : interpreter(interpreter) {
      if (interpreter.call_stack_.size() > 1) {
        saved_call_stack.push_back(interpreter.call_stack_.front());
        interpreter.call_stack_.swap(saved_call_stack);
      }
    }
    ~global_scope () {
      if (saved_call_stack.size() > 0) interpreter.call_stack_.swap(saved_call_stack);
    }
  } scope(*this);

  auto compiled = value->compile(*this, value);
  return compiled->evaluate(*this, compiled);
}

std::shared_ptr<std::string> Interpreter::get_symbol_value (const std::string& name) {
  auto it = static_symbol_values_.find(name);
  if (it != static_symbol_values_.end()) return it->second;

  auto& symbol_value = symbol_values_[name];
  if (!symbol_value.expired()) return std::shared_ptr<std::string>(symbol_value);
  auto new_symbol_value = std::make_shared<std::string>(name);
  symbol_value = new_symbol_value;
  return new_symbol_value;
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

    case '\'': return parse_special(in, loc, token, quote_symbol);
    case '`': return parse_special(in, loc, token, backquote_symbol);
    case ',': return parse_special(in, loc, token, comma_symbol);
    case '@': return parse_special(in, loc, token, comma_at_symbol);
    case '#': return parse_special(in, loc, token, function_symbol);

    default:
      return token.value ? token.value : std::make_shared<Nil>(std::make_shared<location>(token.loc));
  }
}

std::shared_ptr<Value> Interpreter::parse_rest (std::istream& in, location& loc) {
  auto token = lex(in, loc);
  switch (token.character) {
    case ')':
      return std::make_shared<Nil>(std::make_shared<location>(token.loc));

    case '.':
      return parse(in, loc);

    case 0:
      if (!token.value) throw script_error("Unmatched '('", token.loc);
      // fall through

    default: {
      auto first = parse(in, loc, token);
      return std::make_shared<Pair>(first, parse_rest(in, loc), first->loc());
    }
  }
}

std::shared_ptr<Value> Interpreter::parse_special (
    std::istream& in, location& loc, const lexeme& token, const std::shared_ptr<std::string>& symbol_value) {
  auto token_loc = std::make_shared<location>(token.loc);
  return std::make_shared<Pair>(
    std::make_shared<Symbol>(symbol_value, token_loc),
    std::make_shared<Pair>(parse(in, loc), std::make_shared<Nil>(token_loc), token_loc),
    token_loc);
}

lexeme Interpreter::lex (std::istream& in, location& loc) {
  while (loc.good(in)) {
    auto start = loc;
    int ch = loc.get_char(in);
    switch (ch) {
      case std::char_traits<char>::eof():
        break;

      case ';':
        while (loc.good(in) && loc.get_char(in) != '\n'); // consume rest of line
      case '\n':
        break;

      case '"': {
        std::string value;
        while (loc.good(in)) {
          ch = loc.get_char(in);
          switch (ch) {
            case std::char_traits<char>::eof():
              break;

            case '\n':
              value += '\n';
              break;

            case '\\':
              ch = loc.get_char(in);
              switch (ch) {
                case std::char_traits<char>::eof():
                  break;

                case 'n':
                  value += '\n';
                  break;

                case '"':
                  value += '"';
                  break;

                case '\\':
                  value += '\\';
                  break;

                default:
                  if (!std::isspace(ch)) {
                    throw script_error("Unrecognized escape code '" + static_cast<char>(ch) + '\'', loc);
                  }
                  loc.unget_char(in);
                  while (loc.good(in)) { // consume whitespace
                    ch = loc.get_char(in);
                    switch (ch) {
                      case std::char_traits<char>::eof():
                        throw script_error("Unmatched '\"'", start);

                      case '\n':
                        break;

                      default:
                        if (!std::isspace(ch)) {
                          loc.unget_char(in);
                          goto whitespace_ended;
                        }
                        break;
                    }
                  }
                  whitespace_ended:
                  break;
              }
              break;

            case '"':
              return {0, std::make_shared<String>(value, std::make_shared<location>(start)), start};

            default:
              value += static_cast<char>(ch);
              break;
          }
        }
        throw script_error("Unmatched '\"'", start);
      }
      case ',': {
        int next = loc.get_char(in);
        if (next == '@') return {'@', nullptr, start};
        loc.unget_char(in); // fall through
      }
      case '(':
      case ')':
      case '\'':
      case '`':
        return {static_cast<char>(ch), nullptr, start};

      case '#': {
        int next = loc.get_char(in);
        if (next == '\'') return {'#', nullptr, start};
        loc.unget_char(in);
        break;
      }
      case '.': {
        int next = loc.get_char(in);
        loc.unget_char(in);
        if (!std::isdigit(next)) return {'.', nullptr, start};
        // if the next character is a digit, fall through to lex as number
      }
      default: {
        if (std::isspace(ch)) break;
        std::string value(1, static_cast<char>(ch));
        while (loc.good(in)) {
          ch = loc.get_char(in);
          switch (ch) {
            case std::char_traits<char>::eof():
              goto token_ended;

            case ')':
              loc.unget_char(in);
              goto token_ended;

            default:
              if (std::isspace(ch)) {
                loc.unget_char(in);
                goto token_ended;
              }
              value += static_cast<char>(ch);
              break;
          }
        }
        token_ended:
        static std::regex number_pattern("[+-]?(\\d*\\.?\\d+|\\d+\\.?\\d*)");
        return std::regex_match(value, number_pattern)
          ? lexeme{0, std::make_shared<Number>(std::stod(value), std::make_shared<location>(start)), start}
          : value == "t"
          ? lexeme{0, std::make_shared<True>(std::make_shared<location>(start)), start}
          : value == "nil"
          ? lexeme{0, std::make_shared<Nil>(std::make_shared<location>(start)), start}
          : lexeme{0, std::make_shared<Symbol>(get_symbol_value(value), std::make_shared<location>(start)), start};
      }
    }
  }
  return {0, nullptr, loc};
}

std::shared_ptr<Value> Interpreter::lookup_function_binding (const std::shared_ptr<std::string>& symbol_value) const {
  for (auto it = binding_stack_.rbegin(); it != binding_stack_.rend(); ++it) {
    auto pair = it->function.find(symbol_value);
    if (pair != it->function.end()) return pair->second;
  }
  auto pair = static_bindings_.function.find(symbol_value);
  return pair == static_bindings_.function.end() ? nullptr : pair->second;
}

std::shared_ptr<Value> Interpreter::lookup_variable_binding (const std::shared_ptr<std::string>& symbol_value) const {
  for (auto it = binding_stack_.rbegin(); it != binding_stack_.rend(); ++it) {
    auto pair = it->variable.find(symbol_value);
    if (pair != it->variable.end()) return pair->second;
  }
  auto pair = static_bindings_.variable.find(symbol_value);
  return pair == static_bindings_.variable.end() ? nullptr : pair->second;
}

std::shared_ptr<Value> Interpreter::lookup_dynamic_value (const std::shared_ptr<std::string>& symbol_value) const {
  for (auto it = call_stack_.rbegin(); it != call_stack_.rend(); ++it) {
    auto value = (*it)->lookup_dynamic_value(symbol_value);
    if (value) return value;
  }
  return Interpreter::nil();
}

void Interpreter::set_dynamic_value (
    const std::shared_ptr<std::string>& symbol_value, const std::shared_ptr<Value>& value) const {
  for (auto it = call_stack_.rbegin(); it != call_stack_.rend(); ++it) {
    if ((*it)->set_dynamic_value(symbol_value, value)) return;
  }
}

void Value::require_nil () const {
  if (!equals_nil()) throw script_error("Unexpected argument", *loc());
}

double Value::require_number (const location& loc) const {
  auto value = as_number();
  if (!std::isnan(value)) return value;
  throw script_error("Expected number", loc);
}

const std::string& Value::require_string (const location& loc) const {
  throw script_error("Expected string", loc);
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

std::default_random_engine& Value::require_random_state (const location& loc) {
  throw script_error("Expected random state", loc);
}

std::shared_ptr<Value> Value::compile_rest (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  return compile(interpreter, self);
}

std::shared_ptr<Value> Value::compile_commas (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  return self;
}

std::shared_ptr<Value> Value::expand (Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) const {
  return std::make_shared<Pair>(self, pair.right()->compile_rest(interpreter, pair.right()), pair.loc());
}

std::shared_ptr<Value> Value::evaluate_rest (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  return evaluate(interpreter, self);
}

std::shared_ptr<Value> Value::evaluate_commas (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  return self;
}

std::shared_ptr<Value> Value::invoke (Interpreter& interpreter, const Pair& pair) const {
  return apply(interpreter, pair.right()->evaluate_rest(interpreter, pair.right()), *pair.loc());
}

std::shared_ptr<Value> Value::apply (Interpreter& interpreter, const std::shared_ptr<Value>& args, const location& loc) const {
  throw script_error("Expected function", loc);
}

void Value::set_value (Interpreter& interpreter, const std::shared_ptr<Value>& value, const location& loc) const {
  throw script_error("Expected variable", loc);
}

std::shared_ptr<location> Value::empty_loc_ = std::make_shared<location>();

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

std::shared_ptr<Value> Symbol::compile (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  if (value_->size() > 0 && value_->front() == ':') return self;
  auto result = interpreter.lookup_variable_binding(value_);
  if (result) return result;
  throw script_error("Unknown variable \"" + *value_ + '"', *loc());
}

std::shared_ptr<Value> Symbol::apply (Interpreter& interpreter, const std::shared_ptr<Value>& args, const location& loc) const {
  auto result = interpreter.lookup_function_binding(value_);
  if (result) return result->apply(interpreter, args, loc);
  throw script_error("Unknown function \"" + *value_ + '"', loc);
}

std::shared_ptr<Value> Pair::compile (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  auto symbol_value = left_->as_symbol();
  if (!symbol_value) return compile_rest(interpreter, self);
  auto function = interpreter.lookup_function_binding(symbol_value);
  if (function) return function->expand(interpreter, *this, function);
  throw script_error("Unknown function \"" + *symbol_value + '"', *loc());
}

std::shared_ptr<Value> Pair::compile_rest (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  auto left = left_->compile(interpreter, left_);
  auto right = right_->compile_rest(interpreter, right_);
  return std::make_shared<Pair>(left, right, loc());
}

std::shared_ptr<Value> Pair::compile_commas (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  auto left_symbol = left_->as_symbol();
  return left_symbol == comma_symbol || left_symbol == comma_at_symbol
    ? std::make_shared<Pair>(left_, right_->compile_rest(interpreter, right_), loc())
    : std::make_shared<Pair>(left_->compile_commas(interpreter, left_), right_->compile_commas(interpreter, right_), loc());
}

std::shared_ptr<Value> Pair::evaluate (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  return left_->evaluate(interpreter, left_)->invoke(interpreter, *this);
}

std::shared_ptr<Value> Pair::evaluate_rest (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  auto left = left_->evaluate(interpreter, left_);
  auto right = right_->evaluate_rest(interpreter, right_);
  return std::make_shared<Pair>(left, right, loc());
}

std::shared_ptr<Value> Pair::evaluate_commas (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  auto left_symbol = left_->as_symbol();
  if (left_symbol == comma_symbol) {
    auto comma_pair = right_->require_pair(right_);
    comma_pair->right()->require_nil();
    return comma_pair->left()->evaluate(interpreter, comma_pair->left());
  }
  auto left_pair = left_->as_pair(left_);
  if (left_pair && left_pair->left()->as_symbol() == comma_at_symbol) {
    auto comma_pair = left_pair->right()->require_pair(left_pair->right());
    comma_pair->right()->require_nil();

    auto splice = comma_pair->left()->evaluate(interpreter, comma_pair->left());
    if (!*splice) return right_->evaluate_commas(interpreter, right_);

    auto splice_pair = splice->as_pair(splice);
    if (!splice_pair) return std::make_shared<Pair>(splice, right_->evaluate_commas(interpreter, right_), loc());

    auto first = std::make_shared<Pair>(splice_pair->left(), Interpreter::nil(), splice_pair->loc());
    auto last = first;
    auto next = splice_pair->right();
    while (*next) {
      auto next_pair = next->require_pair(next);
      auto new_last = std::make_shared<Pair>(next_pair->left(), Interpreter::nil(), next_pair->loc());
      last->set_right(new_last);
      last = new_last;
      next = next_pair->right();
    }
    last->set_right(right_->evaluate_commas(interpreter, right_));

    return first;
  }
  return std::make_shared<Pair>(
    left_->evaluate_commas(interpreter, left_), right_->evaluate_commas(interpreter, right_), loc());
}

LambdaDefinition::LambdaDefinition (const std::string& name, Interpreter& interpreter, const std::shared_ptr<Value>& args)
    : NamedValue(name) {
  auto arg_pair = args->require_pair(args);
  auto next_param = arg_pair->left();
  bindings ctx;

  auto add_binding = [&](const std::shared_ptr<std::string>& symbol_value) {
    // preserve existing variable bindings in case they're dynamic/constant
    if (!interpreter.lookup_variable_binding(symbol_value)) {
      ctx.variable[symbol_value] = std::make_shared<LexicalVariable>(symbol_value);
    }
  };

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
      aux_params_.push_back({symbol_value, initform->compile(interpreter, initform)});
      add_binding(symbol_value);
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
          process_aux(symbol_value, *loc);
          break;
        }
      }
      key_params_[keyword_symbol] = {symbol_value, initform->compile(interpreter, initform), svar};
      add_binding(symbol_value);
      if (svar) add_binding(svar);
    }
  };

  auto process_rest = [&](const auto& keyword_value, const auto& keyword_loc) {
    if (keyword_value != rest_keyword) {
      process_key(keyword_value, keyword_loc);
      return;
    }
    auto next_param_pair = next_param->require_pair(next_param);
    rest_param_ = next_param_pair->left()->require_symbol(*next_param_pair->loc());
    add_binding(rest_param_);
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
      optional_params_.push_back({symbol_value, initform->compile(interpreter, initform), svar});
      add_binding(symbol_value);
      if (svar) add_binding(svar);
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
    required_params_.push_back(symbol_value);
    add_binding(symbol_value);
  }

  struct bindings_scope {
    Interpreter& interpreter;

    bindings_scope (Interpreter& interpreter, bindings&& ctx) : interpreter(interpreter) {
      interpreter.push_bindings(std::move(ctx));
    }
    ~bindings_scope () { interpreter.pop_bindings(); }

  } scope(interpreter, std::move(ctx));

  body_ = arg_pair->right()->compile_rest(interpreter, arg_pair->right());
}

std::shared_ptr<Value> LambdaDefinition::evaluate (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  return std::make_shared<LambdaFunction>(std::static_pointer_cast<LambdaDefinition>(self), interpreter.current_context());
}

std::shared_ptr<Value> LambdaDefinition::apply_lambda (
    Interpreter& interpreter, const std::shared_ptr<Invocation>& parent_context, const std::shared_ptr<Value>& args) const {
  auto next_arg = args;
  auto ctx = std::make_shared<Invocation>(parent_context);

  for (auto& var : required_params_) {
    auto next_arg_pair = next_arg->require_pair(next_arg);
    ctx->define(var, next_arg_pair->left());
    next_arg = next_arg_pair->right();
  }
  for (auto& optional_param : optional_params_) {
    if (*next_arg) {
      auto next_arg_pair = next_arg->require_pair(next_arg);
      ctx->define(optional_param.var, next_arg_pair->left());
      if (optional_param.svar) ctx->define(optional_param.svar, Interpreter::t());
      next_arg = next_arg_pair->right();

    } else {
      ctx->define(optional_param.var, optional_param.initform->evaluate(interpreter, optional_param.initform));
      if (optional_param.svar) ctx->define(optional_param.svar, Interpreter::nil());
    }
  }
  auto filtered_rest = next_arg;
  if (!key_params_.empty()) {
    filtered_rest = Interpreter::nil();
    std::shared_ptr<Pair> last_filtered;

    while (*next_arg) {
      auto next_arg_pair = next_arg->require_pair(next_arg);
      auto symbol_value = next_arg_pair->left()->as_symbol();
      next_arg = next_arg_pair->right();
      if (symbol_value) {
        auto it = key_params_.find(symbol_value);
        if (it != key_params_.end()) {
          next_arg_pair = next_arg->require_pair(next_arg);
          ctx->define(it->second.var, next_arg_pair->left());
          if (it->second.svar) ctx->define(it->second.svar, Interpreter::t());
          next_arg = next_arg_pair->right();
          continue;
        }
      }
      auto new_last = std::make_shared<Pair>(next_arg_pair->left(), Interpreter::nil());
      if (last_filtered) last_filtered->set_right(new_last);
      else filtered_rest = new_last;
      last_filtered = new_last;
    }

    for (auto& pair : key_params_) {
      if (!ctx->is_defined(pair.second.var)) {
        ctx->define(pair.second.var, pair.second.initform->evaluate(interpreter, pair.second.initform));
        ctx->define(pair.second.svar, Interpreter::nil());
      }
    }
  }
  if (rest_param_) ctx->define(rest_param_, filtered_rest);
  else filtered_rest->require_nil();

  for (auto& aux_param : aux_params_) {
    ctx->define(aux_param.var, aux_param.initform->evaluate(interpreter, aux_param.initform));
  }

  struct frame_scope {
    Interpreter& interpreter;

    frame_scope (Interpreter& interpreter, const std::shared_ptr<Invocation>& ctx) : interpreter(interpreter) {
      interpreter.push_frame(ctx);
    }
    ~frame_scope () { interpreter.pop_frame(); }

  } scope(interpreter, ctx);

  auto last_result = Interpreter::nil();
  auto next_statement = body_;
  while (*next_statement) {
    auto next_statement_pair = next_statement->require_pair(next_statement);
    last_result = next_statement_pair->left()->evaluate(interpreter, next_statement_pair->left());
    next_statement = next_statement_pair->right();
  }

  return last_result;
}

void LambdaFunction::populate (
    const std::shared_ptr<LambdaDefinition>& definition, const std::shared_ptr<Invocation>& parent_context) {
  definition_ = definition;
  parent_context_ = parent_context;
}

std::shared_ptr<Value> LambdaFunction::apply (
    Interpreter& interpreter, const std::shared_ptr<Value>& args, const location& loc) const {
  return definition_->apply_lambda(interpreter, parent_context_, args);
}

std::shared_ptr<Value> ConstantVariable::evaluate (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  return interpreter.top_level_context()->lookup_lexical_value(symbol_value());
}

void ConstantVariable::set_value (Interpreter& interpreter, const std::shared_ptr<Value>& value, const location& loc) const {
  throw script_error("Can't reassign constant \"" + *symbol_value() + '"', loc);
}

std::shared_ptr<Value> LexicalVariable::evaluate (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  return interpreter.current_context()->lookup_lexical_value(symbol_value());
}

void LexicalVariable::set_value (Interpreter& interpreter, const std::shared_ptr<Value>& value, const location& loc) const {
  interpreter.current_context()->set_lexical_value(symbol_value(), value);
}

std::shared_ptr<Value> DynamicVariable::evaluate (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  return interpreter.lookup_dynamic_value(symbol_value());
}

void DynamicVariable::set_value (Interpreter& interpreter, const std::shared_ptr<Value>& value, const location& loc) const {
  interpreter.set_dynamic_value(symbol_value(), value);
}

std::shared_ptr<Value> Macro::expand (Interpreter& interpreter, const Pair& pair, const std::shared_ptr<Value>& self) const {
  auto result = function_->apply(interpreter, pair.right(), *pair.loc());
  return result->compile(interpreter, result);
}

void Invocation::define (const std::shared_ptr<std::string>& symbol_value, const std::shared_ptr<Value>& value) {
  values_[symbol_value] = value;
}

std::shared_ptr<Value> Invocation::lookup_lexical_value (const std::shared_ptr<std::string>& symbol_value) const {
  auto pair = values_.find(symbol_value);
  if (pair != values_.end()) return pair->second;
  return parent_ ? parent_->lookup_lexical_value(symbol_value) : Interpreter::nil();
}

void Invocation::set_lexical_value (const std::shared_ptr<std::string>& symbol_value, const std::shared_ptr<Value>& value) {
  auto pair = values_.find(symbol_value);
  if (pair != values_.end()) pair->second = value;
  else if (parent_) parent_->set_lexical_value(symbol_value, value);
}

std::shared_ptr<Value> Invocation::lookup_dynamic_value (const std::shared_ptr<std::string>& symbol_value) const {
  auto pair = values_.find(symbol_value);
  return pair == values_.end() ? nullptr : pair->second;
}

bool Invocation::set_dynamic_value (const std::shared_ptr<std::string>& symbol_value, const std::shared_ptr<Value>& value) {
  auto pair = values_.find(symbol_value);
  if (pair == values_.end()) return false;
  pair->second = value;
  return true;
}

bool location::good (std::istream& in) const {
  return column_ < contents_->length() || in.good();
}

int location::get_char (std::istream& in) {
  if (column_ >= contents_->length()) {
    column_ = 0;
    contents_->clear();
    while (in.good()) {
      int ch = in.get();
      switch (ch) {
        case std::char_traits<char>::eof():
          goto line_ended;

        case '\n':
          *contents_ += '\n';
          goto line_ended;

        default:
          *contents_ += static_cast<char>(ch);
          break;
      }
    }
    line_ended:
    if (contents_->empty()) return std::char_traits<char>::eof();
  }
  char ch = contents_->at(column_++);
  if (ch == '\n') ++line_;
  return ch;
}

void location::unget_char (std::istream& in) {
  if (column_ > 0) --column_;
  else in.unget();
}

std::string location::to_string () const {
  return *filename_ + " line " + std::to_string(line_ + 1) + ", column " + std::to_string(column_ + 1) + ":\n" +
    (contents_->length() > 0 && contents_->back() == '\n' ? *contents_ : *contents_ + '\n') +
    std::string(column_, ' ') + '^';
}

}
