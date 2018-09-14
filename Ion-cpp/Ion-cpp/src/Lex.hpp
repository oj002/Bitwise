#pragma once
#include "Common.hpp"
#include "stdafx.h"

namespace Ion
{
	extern const char *typedef_keyword;
	extern const char *enum_keyword;
	extern const char *struct_keyword;
	extern const char *union_keyword;
	extern const char *var_keyword;
	extern const char *const_keyword;
	extern const char *func_keyword;
	extern const char *sizeof_keyword;
	extern const char *break_keyword;
	extern const char *continue_keyword;
	extern const char *return_keyword;
	extern const char *if_keyword;
	extern const char *else_keyword;
	extern const char *while_keyword;
	extern const char *do_keyword;
	extern const char *for_keyword;
	extern const char *switch_keyword;
	extern const char *case_keyword;
	extern const char *default_keyword;

	extern const char *first_keyword;
	extern const char *last_keyword;
	extern std::vector<const char *> keywords;

	void init_keywords();

	bool is_keyword_str(const char *str);

	struct Token
	{
		enum Kind
		{
			END_OF_FILE,
			COLON,
			LPAREN,
			RPAREN,
			LBRACE, // '{'
			RBRACE, // '}'
			LBRACKET, // '['
			RBRACKET, // ']'
			COMMA,
			DOT,
			QUESTION,
			SEMICOLON,
			KEYWORD,
			INT,
			FLOAT,
			STR,
			NAME,
			NEG,
			NOT,
			// Multiplicative precedence
			MUL,
			FIRST_MUL = MUL,
			DIV,
			MOD,
			AND,
			LSHIFT,
			RSHIFT,
			LAST_MUL = RSHIFT,
			// Additive precedence
			ADD,
			FIRST_ADD = ADD,
			SUB,
			XOR,
			OR,
			LAST_ADD = OR,
			// Comparative precedence
			EQ,
			FIRST_CMP = EQ,
			NOTEQ,
			LT,
			GT,
			LTEQ,
			GTEQ,
			LAST_CMP = GTEQ,
			AND_AND,
			OR_OR,
			// Assignment operators
			ASSIGN,
			FIRST_ASSIGN = ASSIGN,
			ADD_ASSIGN,
			SUB_ASSIGN,
			OR_ASSIGN,
			AND_ASSIGN,
			XOR_ASSIGN,
			LSHIFT_ASSIGN,
			RSHIFT_ASSIGN,
			MUL_ASSIGN,
			DIV_ASSIGN,
			MOD_ASSIGN,
			LAST_ASSIGN = MOD_ASSIGN,
			INC,
			DEC,
			COLON_ASSIGN,
		} kind;

		enum class Mod
		{
			NONE,
			HEX,
			BIN,
			OCT,
			CHAR
		} mod;

		const char *start;
		const char *end;

		union
		{
			uint64_t int_val;
			double float_val;
			const char *str_val;
			const char *name;
		};
	};

	extern Token token;
	extern const char *stream;

	template<typename T>
	static inline Token::Kind token_cast(T kind)
	{
		return static_cast<Token::Kind>(kind);
	}

	const char *token_kind_name(Token::Kind kind);
	const char *token_info();

	uint8_t char_to_digit(char c);
	void scan_int();
	void scan_float();
	char escape_to_char(char c);
	void scan_char();
	void scan_str();

	void next_token();

	void init_stream(const char *str);

	bool is_token(Token::Kind kind);
	bool is_token_name(const char *name);
	bool is_keyword(const char *name);
	bool match_keyword(const char *name);
	bool match_token(Token::Kind kind);
	bool expect_token(Token::Kind kind);

	void keyword_test();
	void lex_test();
} // namespace Ion

