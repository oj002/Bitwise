#include "Lex.hpp"

namespace Ion
{
	const char *typedef_keyword;
	const char *enum_keyword;
	const char *struct_keyword;
	const char *union_keyword;
	const char *var_keyword;
	const char *const_keyword;
	const char *func_keyword;
	const char *cast_keyword;
	const char *sizeof_keyword;
	const char *break_keyword;
	const char *continue_keyword;
	const char *return_keyword;
	const char *if_keyword;
	const char *else_keyword;
	const char *while_keyword;
	const char *do_keyword;
	const char *for_keyword;
	const char *switch_keyword;
	const char *case_keyword;
	const char *default_keyword;

	const char *first_keyword;
	const char *last_keyword;
	std::vector<const char *> keywords;

	#define KEYWORD(name) name##_keyword = str_intern(#name); keywords.push_back(name##_keyword)

	void init_keywords()
	{
		static bool inited;
		if (inited)
		{
			return;
		}
		char *arena_end = str_arena.end;
		KEYWORD(typedef);
		KEYWORD(enum);
		KEYWORD(struct);
		KEYWORD(union);
		KEYWORD(const);
		KEYWORD(var);
		KEYWORD(func);
		KEYWORD(cast);
		KEYWORD(sizeof);
		KEYWORD(break);
		KEYWORD(continue);
		KEYWORD(return);
		KEYWORD(if);
		KEYWORD(else);
		KEYWORD(while);
		KEYWORD(do);
		KEYWORD(for);
		KEYWORD(switch);
		KEYWORD(case);
		KEYWORD(default);
		assert(str_arena.end == arena_end);
		first_keyword = typedef_keyword;
		last_keyword = default_keyword;
		inited = true;
	}

	#undef KEYWORD
	bool is_keyword_str(const char *str)
	{
		return first_keyword <= str && str <= last_keyword; // Will check if str is in the intern strings between the keywords
	}

	Token token;
	const char *stream;

	const char *token_kind_name(Token::Kind kind)
	{
		switch (kind)
		{
		case Token::END_OF_FILE: return "EOF";
		case Token::COLON: return ":";
		case Token::LPAREN: return "(";
		case Token::RPAREN: return ")";
		case Token::LBRACE: return "{";
		case Token::RBRACE: return "}";
		case Token::LBRACKET: return "[";
		case Token::RBRACKET: return "]";
		case Token::COMMA: return ",";
		case Token::DOT: return ".";
		case Token::QUESTION: return "?";
		case Token::SEMICOLON: return ";";
		case Token::KEYWORD: return "keyword";
		case Token::INT: return "int";
		case Token::FLOAT: return "float";
		case Token::STR: return "string";
		case Token::NAME: return "name";
		case Token::NEG: return "~";
		case Token::NOT: return "!";
		case Token::MUL: return "*";
		case Token::DIV: return "/";
		case Token::MOD: return "%";
		case Token::AND: return "&";
		case Token::LSHIFT: return "<<";
		case Token::RSHIFT: return ">>";
		case Token::ADD: return "+";
		case Token::SUB: return "-";
		case Token::OR: return "|";
		case Token::XOR: return "^";
		case Token::EQ: return "==";
		case Token::NOTEQ: return "!=";
		case Token::LT: return "<";
		case Token::GT: return ">";
		case Token::LTEQ: return "<=";
		case Token::GTEQ: return ">=";
		case Token::AND_AND: return "&&";
		case Token::OR_OR: return "||";
		case Token::ASSIGN: return "=";
		case Token::ADD_ASSIGN: return "+=";
		case Token::SUB_ASSIGN: return "-=";
		case Token::OR_ASSIGN: return "|=";
		case Token::AND_ASSIGN: return "&=";
		case Token::XOR_ASSIGN: return "^=";
		case Token::MUL_ASSIGN: return "*=";
		case Token::DIV_ASSIGN: return "/=";
		case Token::MOD_ASSIGN: return "%=";
		case Token::LSHIFT_ASSIGN: return "<<=";
		case Token::RSHIFT_ASSIGN: return ">>=";
		case Token::INC: return "++";
		case Token::DEC: return "--";
		case Token::COLON_ASSIGN: return ":=";
		}
		assert(0);
		return nullptr;
	};
	const char *token_info()
	{
		if (token.kind == Token::NAME || token.kind == Token::KEYWORD) { return token.name; }
		else { return token_kind_name(token.kind); }
	}
	uint8_t char_to_digit(char c)
	{
		switch (c)
		{
		case '0': return 0;		case '1': return 1;		case '2': return 2;
		case '3': return 3;		case '4': return 4;		case '5': return 5;
		case '6': return 6;		case '7': return 7;		case '8': return 8;
		case '9': return 9;
		case 'a': case 'A': return 10;		case 'b': case 'B': return 11;
		case 'c': case 'C': return 12;		case 'd': case 'D': return 13;
		case 'e': case 'E': return 14;		case 'f': case 'F': return 15;
		}
		return 0;
	}
	void scan_int()
	{
		uint64_t base{ 10 };
		if (*stream == '0')
		{
			++stream;
			if (std::tolower(*stream) == 'x')
			{
				++stream;
				token.mod = Token::Mod::HEX;
				base = 16;
			}
			else if (std::tolower(*stream) == 'b')
			{
				++stream;
				token.mod = Token::Mod::BIN;
				base = 2;
			}
			else if (std::isdigit(*stream) != 0)
			{
				token.mod = Token::Mod::OCT;
				base = 8;
			}
		}

		uint64_t val{ 0 };
		while (true)
		{
			uint64_t digit{ char_to_digit(*stream) };

			if (digit == 0 && *stream != '0') { break; }
			if (digit >= base)
			{
				syntax_error("Digit '%c' out of range for base %" PRIu64, *stream, base);
				digit = 0;
			}
			if (val > (std::numeric_limits<uint64_t>::max() - digit) / base)
			{
				syntax_error("Integer literal overflow");
				while (std::isdigit(*stream)) { ++stream; }
				val = 0; break;
			}
			val = val * base + digit;
			++stream;
		}
		token.kind = Token::INT;
		token.int_val = val;
	}
	void scan_float()
	{
		const char *start{ stream };
		while (std::isdigit(*stream)) { ++stream; }
		if (*stream == '.') { ++stream; }
		while (std::isdigit(*stream)) { ++stream; }

		if (tolower(*stream) == 'e')
		{
			++stream;
			if (*stream == '+' || *stream == '-') { ++stream; }
			if (!std::isdigit(*stream))
			{
				syntax_error("Expected digit after float literal exponent, found '%c'.", *stream);
			}
			while (std::isdigit(*stream)) { ++stream; }
		}
		const double val{ std::strtod(start, nullptr) };
		if (val == HUGE_VAL)
		{
			syntax_error("Float literal overflow");
		}
		token.kind = Token::FLOAT;
		token.float_val = val;
	}
	char escape_to_char(char c)
	{
		switch (c)
		{
		case 'n': return '\n';
		case 'r': return '\r';
		case 't': return '\t';
		case 'v': return '\v';
		case 'b': return '\b';
		case 'a': return '\a';
		case '0': return 0;
		}
		return 0;
	}
	void scan_char()
	{
		assert(*stream == '\''); ++stream;

		char val{ 0 };
		if (*stream == '\'')
		{
			syntax_error("Char literal cannot be empty"); ++stream;
		}
		else if (*stream == '\n')
		{
			syntax_error("Char literal cannot contain newline");
		}
		else if (*stream == '\\')
		{
			++stream;
			val = escape_to_char(*stream);
			if (val == 0 && *stream != '0')
			{
				syntax_error("Invalid char literal escape '\\%c'", *stream);
			}
			++stream;
		}
		else { val = *stream;  ++stream; }

		if (*stream != '\'')
		{
			syntax_error("Expected closing char quote, got '%c'", *stream);
		}
		else { ++stream; }

		token.kind = Token::INT;
		token.int_val = val;
		token.mod = Token::Mod::CHAR;
	}
	void scan_str()
	{
		assert(*stream == '"'); ++stream;
		std::string str;
		while ((*stream != 0) && *stream != '"')
		{
			char val{ *stream };
			if (val == '\n')
			{
				syntax_error("String literal cannot contain newline"); break;
			}
			else if (val == '\\')
			{
				++stream;
				val = escape_to_char(*stream);
				if (val == 0 && *stream != '0')
				{
					syntax_error("Invalid string literal escape '\\%c'", *stream);
				}
			}
			str += val; ++stream;
		}
		if (*stream) { assert(*stream == '"'); ++stream; }
		else { syntax_error("Unexpected end of file within string literal"); }
		token.kind = Token::STR;
		token.str_val = str_intern(str.c_str());
	}

	#define CASE1(c1, k1) \
	case c1: token.kind = Token::k1; ++stream; break;

	#define CASE2(c1, k1, c2, k2) \
	case c1: \
		token.kind = Token::k1; ++stream; \
		if(*stream == c2) { token.kind = Token::k2; ++stream; } \
		break;

	#define CASE3(c1, k1, c2, k2, c3, k3) \
	case c1: \
		token.kind = Token::k1; stream++; \
		if (*stream == c2) { token.kind = Token::k2; stream++; } \
		else if (*stream == c3) { token.kind = Token::k3; stream++; } \
		break;

	void next_token()
	{
	repeat:
		token.start = stream;
		token.mod = Token::Mod::NONE;
		switch (*stream)
		{
		case ' ': case '\t': case '\n': case '\r': case '\v':
		{
			while (std::isspace(*stream)) { ++stream; }
			goto repeat; break;
		}
		case '\'': { scan_char(); break; }
		case '"': { scan_str(); break; }
		case '.':
		{
			if (std::isdigit(stream[1]))
			{
				scan_float();
			}
			else
			{
				token.kind = Token::DOT; ++stream;
			}
			break;
		}
		case '0':  case '1': case '2': case'3': case '4': case '5': case '6': case '7': case '8': case '9':
		{
			while (std::isdigit(*stream)) { stream++; }

			const char c = *stream;
			stream = token.start;
			if (c == '.' || std::tolower(c) == 'e') { scan_float(); }
			else { scan_int(); }
			break;
		}
		case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j':
		case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't':
		case 'u': case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B': case 'C': case 'D':
		case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
		case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
		case 'Y': case 'Z': case '_':
		{
			while (std::isalnum(*stream) || *stream == '_')
			{
				++stream;
			}
			token.name = str_intern_range(token.start, stream);
			token.kind = (is_keyword_str(token.name) ? Token::KEYWORD : Token::NAME);
			break;
		}
		case '<':
		{
			token.kind = Token::LT; ++stream;
			if (*stream == '<')
			{
				token.kind = Token::LSHIFT; ++stream;
				if (*stream == '=')
				{
					token.kind = Token::LSHIFT_ASSIGN; ++stream;
				}
			}
			else if (*stream == '=')
			{
				token.kind = Token::LTEQ; ++stream;
			}
			break;
		}
		case '>':
		{
			token.kind = Token::GT; ++stream;
			if (*stream == '>')
			{
				token.kind = Token::RSHIFT; ++stream;
				if (*stream == '=')
				{
					token.kind = Token::RSHIFT_ASSIGN; ++stream;
				}
			}
			else if (*stream == '=')
			{
				token.kind = Token::GTEQ;
				stream++;
			}
			break;
		}
		CASE1('\0', END_OF_FILE);
		CASE1('(', LPAREN);
		CASE1(')', RPAREN);
		CASE1('{', LBRACE);
		CASE1('}', RBRACE);
		CASE1('[', LBRACKET);
		CASE1(']', RBRACKET);
		CASE1(',', COMMA);
		CASE1('?', QUESTION);
		CASE1(';', SEMICOLON);
		CASE1('~', NEG);
		CASE1('!', NOT);
		CASE2(':', COLON, '=', COLON_ASSIGN);
		CASE2('=', ASSIGN, '=', EQ);
		CASE2('^', XOR, '=', XOR_ASSIGN);
		CASE2('*', MUL, '=', MUL_ASSIGN);
		CASE2('/', DIV, '=', DIV_ASSIGN);
		CASE2('%', MOD, '=', MOD_ASSIGN);
		CASE3('+', ADD, '=', ADD_ASSIGN, '+', INC);
		CASE3('-', SUB, '=', SUB_ASSIGN, '-', DEC);
		CASE3('&', AND, '=', AND_ASSIGN, '&', AND_AND);
		CASE3('|', OR, '=', OR_ASSIGN, '|', OR_OR);
		default:
		{
			syntax_error("Invalid '%c' token, skipping", *stream);
			stream++;
			goto repeat;
		}
		}
		token.end = stream;
	}

	#undef CASE1
	#undef CASE2
	#undef CASE3

	void init_stream(const char *str)
	{
		stream = str;
		next_token();
	}

	bool is_token(Token::Kind kind) { return token.kind == kind; }
	bool is_token_name(const char *name) { return token.kind == Token::NAME && token.name == name; }
	bool is_keyword(const char *name) { return is_token(Token::KEYWORD) && token.name == name; }
	bool match_keyword(const char *name)
	{
		if (is_keyword(name))
		{
			next_token(); return true;
		}
		else { return false; }
	}
	bool match_token(Token::Kind kind)
	{
		if (is_token(kind))
		{
			next_token();
			return true;
		}
		return false;
	}
	bool expect_token(Token::Kind kind)
	{
		if (is_token(kind))
		{
			next_token();
			return true;
		}
		else
		{
			fatal("expected token %s, got %s", token_kind_name(token_cast(kind)), token_info());
			return false;
		}
	}

	void keyword_test()
	{
		init_keywords();
		assert(is_keyword_str(first_keyword));
		assert(is_keyword_str(last_keyword));
		for (const char *str : keywords)
		{
			assert(is_keyword_str(str));
		}
		assert(!is_keyword_str(str_intern("foo")));
	}

	#define assert_token(x) assert(match_token(x))
	#define assert_token_name(x) assert(token.name == str_intern(x) && match_token(Token::NAME))
	#define assert_token_int(x) assert(token.int_val == (x) && match_token(Token::INT))
	#define assert_token_float(x) assert(token.float_val == (x) && match_token(Token::FLOAT))
	#define assert_token_str(x) assert(strcmp(token.str_val, (x)) == 0 && match_token(Token::STR))
	#define assert_token_eof() assert(is_token(Token::END_OF_FILE))

	void lex_test()
	{
		keyword_test();

		// Integer literal tests
		init_stream("0 18446744073709551615 0xffffffffffffffff 042 0b1111");
		assert_token_int(0);
		assert_token_int(18446744073709551615ull);
		assert(token.mod == Token::Mod::HEX);
		assert_token_int(0xffffffffffffffffull);
		assert(token.mod == Token::Mod::OCT);
		assert_token_int(042);
		assert(token.mod == Token::Mod::BIN);
		assert_token_int(0xF);
		assert_token_eof();

		// Float literal tests
		init_stream("3.14 .123 42. 3e10");
		assert_token_float(3.14);
		assert_token_float(.123);
		assert_token_float(42.);
		assert_token_float(3e10);
		assert_token_eof();

		// Char literal tests
		init_stream("'a' '\\n'");
		assert_token_int('a');
		assert_token_int('\n');
		assert_token_eof();

		// String literal tests
		init_stream("\"foo\" \"a\\nb\"");
		assert_token_str("foo");
		assert_token_str("a\nb");
		assert_token_eof();

		// Operator tests
		init_stream(": := + += ++ < <= << <<=");
		assert_token(Token::COLON);
		assert_token(Token::COLON_ASSIGN);
		assert_token(Token::ADD);
		assert_token(Token::ADD_ASSIGN);
		assert_token(Token::INC);
		assert_token(Token::LT);
		assert_token(Token::LTEQ);
		assert_token(Token::LSHIFT);
		assert_token(Token::LSHIFT_ASSIGN);
		assert_token_eof();

		// Misc tests
		init_stream("XY+(XY)_HELLO1,234+994");
		assert_token_name("XY");
		assert_token(Token::ADD);
		assert_token(Token::LPAREN);
		assert_token_name("XY");
		assert_token(Token::RPAREN);
		assert_token_name("_HELLO1");
		assert_token(Token::COMMA);
		assert_token_int(234);
		assert_token(Token::ADD);
		assert_token_int(994);
		assert_token_eof();
	}

	#undef assert_token
	#undef assert_token_name
	#undef assert_token_int
	#undef assert_token_float
	#undef assert_token_str
	#undef assert_token_eof
}