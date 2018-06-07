#pragma once
#include "Lex.hpp"
#include "Ast.hpp"


namespace Ion
{
	static int indent;

	void print_newline();
	void print_typespec(Typespec *type);
	void print_expr(Expr *expr);
	void print_stmt_block(StmtBlock block);
	void print_stmt(Stmt *stmt);
	void print_aggregate_decl(Decl *decl);
	void print_decl(Decl *decl);
	void print_test();
}