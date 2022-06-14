#include <cctype>
#include <fstream>
#include <sstream>

#include "goldberg.hpp"

namespace goldberg {

std::shared_ptr<Value> Interpreter::t_ = std::make_shared<True>();
std::shared_ptr<Value> Interpreter::nil_ = std::make_shared<Nil>();

std::unordered_map<std::string, std::shared_ptr<std::string>> Interpreter::static_symbol_values_;

bindings Interpreter::static_bindings_ = create_static_bindings();

std::shared_ptr<std::string> Interpreter::static_symbol_value (const std::string& value) {
  auto& pointer = static_symbol_values_[value];
  if (!pointer) pointer = std::make_shared<std::string>(value);
  return pointer;
}

Interpreter::Interpreter () {
  push_frame(std::make_shared<Invocation>());
}

const std::shared_ptr<Invocation>& Interpreter::current_context () const {
  return call_stack_.back().context;
}

std::shared_ptr<Value> Interpreter::evaluate (const std::string& string, const std::string& filename) {
  std::istringstream in(string);
  return load(in, filename);
}

std::shared_ptr<Value> Interpreter::require (const std::string& filename) {
  auto& value = required_files_[filename];
  if (!value) value = load(filename);
  return value;
}

std::shared_ptr<Value> Interpreter::load (const std::string& filename) {
  std::ifstream in(filename);
  return load(in, filename);
}

std::shared_ptr<Value> Interpreter::load (std::istream& in, const std::string& filename) {
  location loc{std::make_shared<std::string>(filename), 1, 1};
  std::shared_ptr<Value> last_result = std::make_shared<Nil>(std::make_shared<location>(loc));
  while (in) {
    auto parsed = parse(in, loc, this);
    auto compiled = parsed->compile(*this, parsed);
    auto result = compiled->evaluate(*this, compiled);
    if (*result) last_result = result;
  }
  return last_result;
}

std::shared_ptr<Value> Interpreter::parse (const std::string& string) {
  std::istringstream in(string);
  location loc{std::make_shared<std::string>("<string>"), 1, 1};
  std::shared_ptr<Value> last_result = std::make_shared<Nil>(std::make_shared<location>(loc));
  while (in) {
    auto result = parse(in, loc);
    if (*result) last_result = result;
  }
  return last_result;
}

std::shared_ptr<Value> Interpreter::parse (std::istream& in, location& loc, Interpreter* interpreter) {
  return parse(in, loc, lex(in, loc, interpreter), interpreter);
}

static auto quote_symbol = Interpreter::static_symbol_value("quote");
static auto backquote_symbol = Interpreter::static_symbol_value("backquote");
static auto comma_symbol = Interpreter::static_symbol_value("comma");

std::shared_ptr<Value> Interpreter::parse (std::istream& in, location& loc, const lexeme& token, Interpreter* interpreter) {
  switch (token.character) {
    case '.':
      throw script_error("Unexpected '.'", token.loc);

    case '(':
      return parse_rest(in, loc, interpreter);

    case ')':
      throw script_error("Mismatched ')'", token.loc);

    case '\'': {
      auto token_loc = std::make_shared<location>(token.loc);
      return std::make_shared<Pair>(
        std::make_shared<Symbol>(quote_symbol, token_loc),
        std::make_shared<Pair>(parse(in, loc, interpreter), std::make_shared<Nil>(token_loc), token_loc),
        token_loc);
    }
    case '`': {
      auto token_loc = std::make_shared<location>(token.loc);
      return std::make_shared<Pair>(
        std::make_shared<Symbol>(backquote_symbol, token_loc),
        std::make_shared<Pair>(parse(in, loc, interpreter), std::make_shared<Nil>(token_loc), token_loc),
        token_loc);
    }
    case ',': {
      auto token_loc = std::make_shared<location>(token.loc);
      return std::make_shared<Pair>(
        std::make_shared<Symbol>(comma_symbol, token_loc),
        std::make_shared<Pair>(parse(in, loc, interpreter), std::make_shared<Nil>(token_loc), token_loc),
        token_loc);
    }
    default:
      return token.value;
  }
}

std::shared_ptr<Value> Interpreter::parse_rest (std::istream& in, location& loc, Interpreter* interpreter) {
  auto token = lex(in, loc, interpreter);
  switch (token.character) {
    case ')':
      return std::make_shared<Nil>(std::make_shared<location>(token.loc));

    case '.':
      return parse(in, loc, interpreter);

    case 0:
      if (!*token.value) throw script_error("Unmatched '('", token.loc);
      // fall through

    default: {
      auto first = parse(in, loc, token, interpreter);
      return std::make_shared<Pair>(first, parse_rest(in, loc, interpreter), first->loc());
    }
  }
}

lexeme Interpreter::lex (std::istream& in, location& loc, Interpreter* interpreter) {
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
      case '\'':
      case '`':
      case ',': {
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
          : lexeme{0, std::make_shared<Symbol>(get_symbol_value(value, interpreter), std::make_shared<location>(start)), start};
      }
    }
  }
  return {0, std::make_shared<Nil>(std::make_shared<location>(loc)), loc};
}

std::shared_ptr<std::string> Interpreter::get_symbol_value (const std::string& value, Interpreter* interpreter) {
  if (!interpreter) return static_symbol_value(value);

  auto it = static_symbol_values_.find(value);
  if (it != static_symbol_values_.end()) return it->second;

  auto& symbol_value = interpreter->symbol_values_[value];
  if (!symbol_value.expired()) return std::shared_ptr<std::string>(symbol_value);
  auto new_symbol_value = std::make_shared<std::string>(value);
  symbol_value = new_symbol_value;
  return new_symbol_value;
}

std::shared_ptr<Value> Interpreter::lookup (const std::shared_ptr<std::string>& symbol_value) const {
  for (auto it = binding_stack_.rbegin(); it != binding_stack_.rend(); ++it) {
    auto pair = it->find(symbol_value);
    if (pair != it->end()) return pair->second;
  }
  auto pair = static_bindings_.find(symbol_value);
  return pair == static_bindings_.end() ? nullptr : pair->second;
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

std::string Value::require_string (const location& loc) const {
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
  throw script_error("Expected function", *pair.loc());
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

std::shared_ptr<Value> Symbol::compile (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  if (value_->size() > 0 && value_->front() == ':') return self;
  auto result = interpreter.lookup(value_);
  if (result) return result;
  throw script_error("Unknown symbol \"" + *value_ + '"', *loc());
}

std::shared_ptr<Value> Pair::compile (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  auto left = left_->compile(interpreter, left_);
  return left->expand(interpreter, *this, left);
}

std::shared_ptr<Value> Pair::compile_rest (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  auto left = left_->compile(interpreter, left_);
  auto right = right_->compile_rest(interpreter, right_);
  return std::make_shared<Pair>(left, right, loc());
}

std::shared_ptr<Value> Pair::compile_commas (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  return left_->as_symbol() == comma_symbol
    ? std::make_shared<Pair>(left_, right_->compile(interpreter, right_), loc())
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
  if (left_->as_symbol() == comma_symbol) {
    auto comma_pair = right_->require_pair(right_);
    comma_pair->right()->require_nil();
    return comma_pair->left()->evaluate(interpreter, comma_pair->left());
  }
  return std::make_shared<Pair>(
    left_->evaluate_commas(interpreter, left_), right_->evaluate_commas(interpreter, right_), loc());
}

static auto aux_keyword = Interpreter::static_symbol_value("&aux");
static auto key_keyword = Interpreter::static_symbol_value("&key");
static auto rest_keyword = Interpreter::static_symbol_value("&rest");
static auto optional_keyword = Interpreter::static_symbol_value("&optional");

void parameters::init (const std::shared_ptr<Value>& spec, Interpreter* interpreter) {
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
      aux.push_back({symbol_value, initform});
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
          keyword_symbol = Interpreter::get_symbol_value(':' + *symbol_value, interpreter);
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
        keyword_symbol = Interpreter::get_symbol_value(':' + *symbol_value, interpreter);
        if (is_keyword(symbol_value)) {
          process_aux(symbol_value, *loc);
          break;
        }
      }
      key[keyword_symbol] = {symbol_value, initform, svar};
    }
  };

  auto process_rest = [&](const auto& keyword_value, const auto& keyword_loc) {
    if (keyword_value != rest_keyword) {
      process_key(keyword_value, keyword_loc);
      return;
    }
    auto next_param_pair = next_param->require_pair(next_param);
    rest = next_param_pair->left()->require_symbol(*next_param_pair->loc());
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
      optional.push_back({symbol_value, initform, svar});
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
    required.push_back(symbol_value);
  }
}

void parameters::bind (
    Interpreter& interpreter, const std::shared_ptr<Value>& args, const std::shared_ptr<Invocation>& ctx) const {
  auto next_arg = args;

  for (auto& var : required) {
    auto next_arg_pair = next_arg->require_pair(next_arg);
    ctx->define(var, next_arg_pair->left());
    next_arg = next_arg_pair->right();
  }
  for (auto& optional_param : optional) {
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
  if (!key.empty()) {
    filtered_rest = Interpreter::nil();
    std::shared_ptr<Pair> last_filtered;

    while (*next_arg) {
      auto next_arg_pair = next_arg->require_pair(next_arg);
      auto symbol_value = next_arg_pair->left()->as_symbol();
      next_arg = next_arg_pair->right();
      if (symbol_value) {
        auto it = key.find(symbol_value);
        if (it != key.end()) {
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

    for (auto& pair : key) {
      if (!ctx->is_defined(pair.second.var)) {
        ctx->define(pair.second.var, pair.second.initform->evaluate(interpreter, pair.second.initform));
        ctx->define(pair.second.svar, Interpreter::nil());
      }
    }
  }
  if (rest) ctx->define(rest, filtered_rest);
  else filtered_rest->require_nil();

  for (auto& aux_param : aux) {
    ctx->define(aux_param.var, aux_param.initform->evaluate(interpreter, aux_param.initform));
  }
}

LambdaDefinition::LambdaDefinition (const std::string& name, Interpreter& interpreter, const std::shared_ptr<Value>& args)
    : NamedValue(name) {
  auto arg_pair = args->require_pair(args);
  params_.init(arg_pair->left(), &interpreter);

  bindings ctx;
  for (auto& var : params_.required) ctx[var] = std::make_shared<Variable>(var);
  for (auto& optional_param : params_.optional) {
    ctx[optional_param.var] = std::make_shared<Variable>(optional_param.var);
    if (optional_param.svar) ctx[optional_param.svar] = std::make_shared<Variable>(optional_param.svar);
    optional_param.initform = optional_param.initform->compile(interpreter, optional_param.initform);
  }
  if (params_.rest) ctx[params_.rest] = std::make_shared<Variable>(params_.rest);
  for (auto& pair : params_.key) {
    ctx[pair.second.var] = std::make_shared<Variable>(pair.second.var);
    if (pair.second.svar) ctx[pair.second.svar] = std::make_shared<Variable>(pair.second.svar);
    pair.second.initform = pair.second.initform->compile(interpreter, pair.second.initform);
  }
  for (auto& aux_param : params_.aux) {
    ctx[aux_param.var] = std::make_shared<Variable>(aux_param.var);
    aux_param.initform = aux_param.initform->compile(interpreter, aux_param.initform);
  }

  struct bindings_scope {
    Interpreter& interpreter;

    bindings_scope (Interpreter& interpreter, bindings&& ctx) : interpreter(interpreter) {
      interpreter.push_bindings(std::move(ctx)); }
    ~bindings_scope () { interpreter.pop_bindings(); }

  } scope(interpreter, std::move(ctx));

  body_ = arg_pair->right()->compile_rest(interpreter, arg_pair->right());
}

std::shared_ptr<Value> LambdaDefinition::evaluate (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  return std::make_shared<LambdaFunction>(std::static_pointer_cast<LambdaDefinition>(self), interpreter.current_context());
}

std::shared_ptr<Value> LambdaFunction::invoke (Interpreter& interpreter, const Pair& pair) const {
  auto ctx = std::make_shared<Invocation>(parent_context_);
  definition_->params().bind(interpreter, pair.right()->evaluate_rest(interpreter, pair.right()), ctx);

  struct frame_scope {
    Interpreter& interpreter;

    frame_scope (Interpreter& interpreter, const std::shared_ptr<Invocation>& ctx) : interpreter(interpreter) {
      interpreter.push_frame(ctx); }
    ~frame_scope () { interpreter.pop_frame(); }

  } scope(interpreter, ctx);

  auto last_result = Interpreter::nil();
  auto next_statement = definition_->body();
  while (*next_statement) {
    auto next_statement_pair = next_statement->require_pair(next_statement);
    last_result = next_statement_pair->left()->evaluate(interpreter, next_statement_pair->left());
    next_statement = next_statement_pair->right();
  }

  return last_result;
}

std::shared_ptr<Value> Variable::evaluate (Interpreter& interpreter, const std::shared_ptr<Value>& self) const {
  return interpreter.current_context()->lookup(symbol_value_);
}

Macro::Macro (const std::string& name, const std::shared_ptr<Value>& definition) : NamedValue(name) {
  auto definition_pair = definition->require_pair(definition);
  params_.init(definition_pair->left());
  body_ = definition_pair->right();
}

void Invocation::define (const std::shared_ptr<std::string>& symbol_value, const std::shared_ptr<Value>& value) {
  values_[symbol_value] = value;
}

std::shared_ptr<Value> Invocation::lookup (const std::shared_ptr<std::string>& symbol_value) const {
  auto pair = values_.find(symbol_value);
  if (pair != values_.end()) return pair->second;
  return parent_ ? parent_->lookup(symbol_value) : Interpreter::nil();
}

}
