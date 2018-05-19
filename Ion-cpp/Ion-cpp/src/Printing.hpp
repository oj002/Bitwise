#pragma once
#include "Lex.hpp"
#include "Ast.hpp"


namespace Ion
{
	static void print_expr(Expr *expr);
	static void print_stmt(Stmt *stmt);
	static void print_decl(Decl *expr);

	static int indent;

	static void print_newline()
	{
		std::printf("\n%.*s", 2 * indent, "                                                                      ");
	}

	static void print_typespec(Typespec *type)
	{
		switch (Typespec *t{ type }; t->kind)
		{
		case Typespec::NAME: std::printf("%s", t->name); break;
		case Typespec::FUNC:
			std::printf("(func (");
			for (Typespec *it : t->func.args)
			{
				std::printf(" ");
				print_typespec(it);
			}
			std::printf(") ");
			print_typespec(t->func.ret);
			std::printf(")"); break;
		case Typespec::ARRAY:
			std::printf("(array ");
			print_typespec(t->array.elem);
			std::printf(" ");
			print_expr(t->array.size);
			std::printf(")"); break;
		case Typespec::PTR:
			std::printf("(ptr ");
			print_typespec(t->ptr.elem);
			std::printf(")"); break;
		default: assert(0); break;
		}
	}

	static void print_expr(Expr *expr)
	{
		switch (Expr *e{ expr }; e->kind)
		{
		case Expr::INT: std::printf("%" PRIu64, e->int_val); break;
		case Expr::FLOAT: std::printf("%f", e->float_val); break;
		case Expr::STR: std::printf("\"%s\"", e->str_val); break;
		case Expr::NAME: std::printf("%s", e->name); break;
		case Expr::SIZEOF_EXPR:
			std::printf("(sizeof-expr ");
			print_expr(e->sizeof_expr);
			std::printf(")"); break;
		case Expr::SIZEOF_TYPE:
			std::printf("(sizeof-type ");
			print_typespec(e->sizeof_type);
			std::printf(")"); break;
		case Expr::CAST:
			std::printf("(cast "); print_typespec(e->cast.type);
			std::printf(" "); print_expr(e->cast.expr);
			std::printf(")"); break;
		case Expr::CALL:
			std::printf("("); print_expr(e->call.expr);
			for (Expr *it : e->call.args)
			{
				std::printf(" "); print_expr(it);
			}
			std::printf(")"); break;
		case Expr::INDEX:
			std::printf("(index"); print_expr(e->index.expr);
			std::printf(" "); print_expr(e->index.index);
			std::printf(")"); break;
		case Expr::FIELD:
			std::printf("(field "); print_expr(e->field.expr);
			std::printf(" %s)", e->field.name); break;
		case Expr::COMPOUND:
			std::printf("(compound ");
			if (e->compound.type) { print_typespec(e->compound.type); }
			else { std::printf("nil"); }
			for (Expr *it : e->compound.args)
			{
				std::printf(" "); print_expr(it);
			}
			std::printf(")"); break;
		case Expr::UNARY:
			std::printf("(%s ", token_kind_name(e->binary.op));
			print_expr(e->unary.expr);
			std::printf(")"); break;
		case Expr::BINARY:
			std::printf("(%s ", token_kind_name(e->unary.op));
			print_expr(e->binary.left);
			std::printf(" ");
			print_expr(e->binary.right);
			std::printf(")"); break;
		case Expr::TERNARY:
			std::printf("(? "); print_expr(e->ternary.cond);
			std::printf(" "); print_expr(e->ternary.then_expr);
			std::printf(" "); print_expr(e->ternary.else_expr);
			std::printf(")"); break;
		default: assert(0); break;
		}
	}

	static void print_stmt_block(StmtBlock block)
	{
		std::printf("(block");
		++indent;
		for (Stmt *it : block.stmts)
		{
			print_newline();
			print_stmt(it);
		}
		--indent; std::printf(")");
	}
	static void print_stmt(Stmt *stmt)
	{
		switch (Stmt *s{ stmt }; s->kind)
		{
		case Stmt::RETURN:
			std::printf("(return "); print_expr(s->return_stmt.expr);
			std::printf(")"); break;
		case Stmt::BREAK: std::printf("(break)"); break;
		case Stmt::CONTINUE: std::printf("(continue)"); break;
		case Stmt::BLOCK: print_stmt_block(s->block); break;
		case Stmt::IF:
			std::printf("(if "); print_expr(s->if_stmt.cond);
			++indent; print_newline();
			print_stmt_block(s->if_stmt.then_block);
			for (ElseIf &it : s->if_stmt.elseifs)
			{
				print_newline(); std::printf("elseif ");
				print_expr(it.cond); print_newline();
				print_stmt_block(it.block);
			}
			if (!s->if_stmt.else_block.stmts.empty())
			{
				print_newline(); std::printf("else ");
				print_newline();
				print_stmt_block(s->if_stmt.else_block);
			}
			--indent;
			std::printf(")"); break;
		case Stmt::WHILE:
			std::printf("(while "); print_expr(s->while_stmt.cond);
			++indent; print_newline();
			print_stmt_block(s->while_stmt.block); --indent;
			std::printf(")"); break;
		case Stmt::DO_WHILE:
			std::printf("(do-while "); print_expr(s->while_stmt.cond);
			++indent; print_newline();
			print_stmt_block(s->while_stmt.block); --indent;
			std::printf(")"); break;
		case Stmt::FOR:
			std::printf("(for ");
			print_stmt(s->for_stmt.init);
			print_expr(s->for_stmt.cond);
			print_stmt(s->for_stmt.next);
			++indent;
			print_newline();
			print_stmt_block(s->for_stmt.block);
			--indent;
			std::printf(")"); break;
		case Stmt::SWITCH:
			std::printf("(switch ");
			print_expr(s->switch_stmt.expr);
			++indent;
			for (SwitchCase &it : s->switch_stmt.cases)
			{
				print_newline();
				std::printf("(case (%s", it.is_default ? " default" : "");
				for (Expr *exprIt : it.exprs)
				{
					std::printf(" "); print_expr(exprIt);
				}
				std::printf(" ) ");
				++indent; 
				print_newline(); print_stmt_block(it.block);
				--indent;
			}
			--indent;
			std::printf(")"); break;
		case Stmt::ASSIGN:
			std::printf("(%s ", token_kind_name(s->assign.op));
			print_expr(s->assign.left);
			if (s->assign.right)
			{
				std::printf(" "); print_expr(s->assign.right);
			}
			std::printf(")"); break;
		case Stmt::INIT:
			std::printf("(:= %s ", s->init.name);
			print_expr(s->init.expr);
			std::printf(")"); break;
		case Stmt::EXPR: print_expr(s->expr); break;
		default: assert(0); break;
		}
	}

	static void print_aggregate_decl(Decl *decl)
	{
		Decl *d{ decl };
		for (AggregateItem &it : d->aggregate.items)
		{
			print_newline();
			std::printf("("); print_typespec(it.type);
			for (const char *name : it.names)
			{
				std::printf(" %s", name);
			}
			std::printf(")");
		}
	}
	static void print_decl(Decl *decl)
	{
		Decl *d = decl;
		switch (d->kind)
		{
		case Decl::ENUM:
			std::printf("(enum %s", d->name);
			indent++;
			for (EnumItem &it : d->enum_decl.items)
			{
				print_newline();
				std::printf("(%s ", it.name);
				if (it.expr) { print_expr(it.expr); }
				else { std::printf("nil"); }
				std::printf(")");
			}
			indent--;
			std::printf(")");
			break;
		case Decl::STRUCT:
			std::printf("(struct %s", d->name);
			indent++;
			print_aggregate_decl(d);
			indent--;
			std::printf(")");
			break;
		case Decl::UNION:
			std::printf("(union %s", d->name);
			indent++;
			print_aggregate_decl(d);
			indent--;
			std::printf(")");
			break;
		case Decl::VAR:
			std::printf("(var %s ", d->name);
			if (d->var.type) { print_typespec(d->var.type); }
			else { std::printf("nil"); }
			std::printf(" ");
			print_expr(d->var.expr);
			std::printf(")");
			break;
		case Decl::CONST:
			std::printf("(const %s ", d->name);
			print_expr(d->const_decl.expr);
			std::printf(")");
			break;
		case Decl::TYPEDEF:
			std::printf("(typedef %s ", d->name);
			print_typespec(d->typedef_decl.type);
			std::printf(")");
			break;
		case Decl::FUNC:
			std::printf("(func %s ", d->name);
			std::printf("(");
			for (FuncParam &it : d->func.params)
			{
				std::printf(" %s ", it.name);
				print_typespec(it.type);
			}
			std::printf(" ) ");
			if (d->func.ret_type) { print_typespec(d->func.ret_type); }
			else { std::printf("nil"); }
			indent++;
			print_newline();
			print_stmt_block(d->func.block);
			indent--;
			std::printf(")");
			break;
		default: assert(0); break;
		}
	}
	static void print_test()
	{
		std::vector<Expr*> exprs = {
			expr_binary(token_cast('+'), expr_int(1), expr_int(2)),
			expr_unary(token_cast('-'), expr_float(3.14)),
			expr_ternary(expr_name("flag"), expr_str("true"), expr_str("false")),
			expr_field(expr_name("person"), "name"),
			expr_call(expr_name("fact"), std::vector<Expr*> { expr_int(42) }),
			expr_index(expr_field(expr_name("person"), "siblings"), expr_int(3)),
			expr_cast(typespec_ptr(typespec_name("int")), expr_name("void_ptr")),
			expr_compound(typespec_name("Vector"), std::vector<Expr*> { expr_int(1), expr_int(2) })
		};
		for (Expr *it : exprs)
		{
			print_expr(it); std::printf("\n");
		}

		std::vector<Stmt *> stmts = {
			stmt_return(expr_int(42)), stmt_break(), stmt_continue(),
			stmt_block(StmtBlock { std::vector<Stmt*> { stmt_break(), stmt_continue() }, }),
			stmt_expr(expr_call(expr_name("print"), std::vector<Expr*> { expr_int(1), expr_int(2) })),
			stmt_init("x", expr_int(42)), stmt_if(expr_name("flag1"), StmtBlock { std::vector<Stmt*> {
			stmt_return(expr_int(1)) }, },
			std::vector<ElseIf> { {expr_name("flag2"),
			StmtBlock { std::vector<Stmt*> { stmt_return(expr_int(2)) } } } },
			StmtBlock { std::vector<Stmt*> { stmt_return(expr_int(3)) }}
			),
			stmt_while(expr_name("running"),
			StmtBlock { std::vector<Stmt*> {
			stmt_assign(Token::ADD_ASSIGN, expr_name("i"), expr_int(16)) } }),
			stmt_switch(expr_name("val"), std::vector<SwitchCase> { { {
			std::vector<Expr*> { expr_int(3), expr_int(4) },
			false, StmtBlock { std::vector<Stmt*> { stmt_return(expr_name("val")) }, }, },
			{ std::vector<Expr*>{expr_int(1)}, true, StmtBlock { std::vector<Stmt*> {
			stmt_return(expr_int(0)) }} } } })
		};
		for (Stmt *it : stmts) 
		{
			print_stmt(it); std::printf("\n");
		}
	}
}