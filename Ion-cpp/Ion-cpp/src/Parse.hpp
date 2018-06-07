#pragma once
#include "Printing.hpp"

namespace Ion
{
	Typespec *parse_type_func();
	Typespec *parse_type_base();
	Typespec *parse_type();

	Expr *parse_expr_compound(Typespec *type);
	Expr *parse_expr_operand();
	Expr *parse_expr_base();
	bool is_unary_op();
	Expr *parse_expr_unary();
	bool is_mul_op();
	Expr *parse_expr_mul();
	bool is_add_op();
	Expr *parse_expr_add();
	bool is_cmp_op();
	Expr *parse_expr_cmp();
	Expr *parse_expr_and();
	Expr *parse_expr_or();
	Expr *parse_expr_ternary();
	Expr *parse_expr();
	Expr *parse_paren_expr();
	StmtBlock parse_stmt_block();
	Stmt *parse_stmt_if();

	Stmt *parse_stmt_while();

	Stmt *parse_stmt_do_while();
	bool is_assign_op();
	Stmt *parse_simple_stmt();
	Stmt *parse_stmt_for();
	SwitchCase parse_stmt_switch_case();
	Stmt *parse_stmt_switch();
	Stmt *parse_stmt();

	const char *parse_name();
	EnumItem parse_decl_enum_item();
	Decl *parse_decl_enum();
	AggregateItem parse_decl_aggregate_item();
	Decl *parse_decl_aggregate(Decl::Kind kind);
	Decl *parse_decl_var();
	Decl *parse_decl_const();
	Decl *parse_decl_typedef();
	FuncParam parse_decl_func_param();

	Decl *parse_decl_func();

	Decl *parse_decl();

	void parse_and_print_decl(const char *str);

	void parse_test();
}