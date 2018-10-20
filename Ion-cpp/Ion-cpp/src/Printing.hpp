#pragma once
#include "Lex.hpp"
#include "Ast.hpp"


#define PRINTF(...) (Ion::use_print_buf ? (void)Ion::buff_printf(__VA_ARGS__) : (void)std::printf(__VA_ARGS__))
namespace Ion
{
	static int indent;
	static std::string print_buf;
	static bool use_print_buf = false;

	void buff_printf(const char* fmt, ...);
	FILE *flush_print_buff(FILE *file);
	void print_newline();
	void print_typespec(Typespec *type);
	void print_expr(Expr *expr);
	void print_stmt_block(StmtList block);
	void print_stmt(Stmt *stmt);
	void print_aggregate_decl(Decl *decl);
	void print_decl(Decl *decl);
	void print_test();
}