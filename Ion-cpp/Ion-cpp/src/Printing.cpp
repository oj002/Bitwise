#include "Printing.hpp"
#include <stdarg.h>


namespace Ion
{
	void buff_printf(const char* fmt, ...)
	{
		va_list args;
		va_start(args, fmt);


		char buf[1024];
		int size = std::vsnprintf(buf, 256, fmt, args);
		if (size < 256)
			print_buf += buf;
		else
		{
			char *newBuf = (char*)malloc(size * sizeof(char));
			std::vsnprintf(newBuf, size, fmt, args);
			print_buf += newBuf;
			free(newBuf);
		}


		va_end(args);
	}

	FILE *flush_print_buff(FILE *file)
	{
		if (!print_buf.empty())
		{
			if (file)
				fputs(print_buf.c_str(), file);
			
			print_buf.clear();
		}
		return file;
	}

	void print_newline()
	{
		PRINTF("\n%.*s", 2 * indent, "                                                                      ");
	}

	void print_typespec(Typespec *type)
	{
		switch (Typespec *t{ type }; t->kind)
		{
		case Typespec::NAME: PRINTF("%s", t->name); break;
		case Typespec::FUNC:
			PRINTF("(func (");
			for (Typespec *it : t->func.args)
			{
				PRINTF(" ");
				print_typespec(it);
			}
			PRINTF(" ) ");
			if (t->func.ret) print_typespec(t->func.ret);
			else PRINTF("void");
			PRINTF(")"); break;
		case Typespec::ARRAY:
			PRINTF("(array ");
			print_typespec(t->array.elem);
			PRINTF(" ");
			print_expr(t->array.size);
			PRINTF(")"); break;
		case Typespec::PTR:
			PRINTF("(ptr ");
			print_typespec(t->ptr.elem);
			PRINTF(")"); break;
		default: assert(0); break;
		}
	}

	void print_expr(Expr *expr)
	{
		switch (Expr *e{ expr }; e->kind)
		{
		case Expr::INT: PRINTF("%" PRIu64, e->int_val); break;
		case Expr::FLOAT: PRINTF("%f", e->float_val); break;
		case Expr::STR: PRINTF("\"%s\"", e->str_val); break;
		case Expr::NAME: PRINTF("%s", e->name); break;
		case Expr::SIZEOF_EXPR:
			PRINTF("(sizeof-expr ");
			print_expr(e->sizeof_expr);
			PRINTF(")"); break;
		case Expr::SIZEOF_TYPE:
			PRINTF("(sizeof-type ");
			print_typespec(e->sizeof_type);
			PRINTF(")"); break;
		case Expr::CAST:
			PRINTF("(cast "); print_typespec(e->cast.type);
			PRINTF(" "); print_expr(e->cast.expr);
			PRINTF(")"); break;
		case Expr::CALL:
			PRINTF("("); print_expr(e->call.expr);
			for (Expr *it : e->call.args) {
				PRINTF(" "); print_expr(it);
			}
			PRINTF(")"); break;
		case Expr::INDEX:
			PRINTF("(index "); print_expr(e->index.expr);
			PRINTF(" "); print_expr(e->index.index);
			PRINTF(")"); break;
		case Expr::FIELD:
			PRINTF("(field "); print_expr(e->field.expr);
			PRINTF(" %s)", e->field.name); break;
		case Expr::COMPOUND:
			PRINTF("(compound ");
			if (e->compound.type)
				print_typespec(e->compound.type);
			else
				PRINTF("nil");
			for (auto &it : e->compound.fields)
			{
				PRINTF(" ");
				if (it.kind == CompoundField::DEFAULT)
					PRINTF("(nil ");
				else if (it.kind == CompoundField::NAME)
					PRINTF("(name %s ", it.name);
				else 
				{
					assert(it.kind == CompoundField::INDEX);
					PRINTF("(index ");
					print_expr(it.index);
					PRINTF(" ");
				}
				print_expr(it.init);
				PRINTF(")");
			}
			PRINTF(")");
			break;
		case Expr::UNARY:
			PRINTF("(%s ", token_kind_name(e->binary.op));
			print_expr(e->unary.expr);
			PRINTF(")"); break;
		case Expr::BINARY:
			PRINTF("(%s ", token_kind_name(e->unary.op));
			print_expr(e->binary.left);
			PRINTF(" ");
			print_expr(e->binary.right);
			PRINTF(")"); break;
		case Expr::TERNARY:
			PRINTF("(? "); print_expr(e->ternary.cond);
			PRINTF(" "); print_expr(e->ternary.then_expr);
			PRINTF(" "); print_expr(e->ternary.else_expr);
			PRINTF(")"); break;
		default: assert(0); break;
		}
	}

	void print_stmt_block(StmtList block)
	{
		PRINTF("(block");
		++indent;
		for (Stmt *it : block.stmts)
		{
			print_newline();
			print_stmt(it);
		}
		--indent; PRINTF(")");
	}
	void print_stmt(Stmt *stmt)
	{
		switch (Stmt *s{ stmt }; s->kind)
		{
		case Stmt::DECL: print_decl(stmt->decl);
		case Stmt::RETURN:
			PRINTF("(return");
			if (s->expr)
			{
				PRINTF(" ");
				print_expr(s->expr);
			}
			PRINTF(")"); break;
		case Stmt::BREAK: PRINTF("(break)"); break;
		case Stmt::CONTINUE: PRINTF("(continue)"); break;
		case Stmt::BLOCK: print_stmt_block(s->block); break;
		case Stmt::IF:
			PRINTF("(if "); print_expr(s->if_stmt.cond);
			++indent; print_newline();
			print_stmt_block(s->if_stmt.then_block);
			for (ElseIf &it : s->if_stmt.elseifs)
			{
				print_newline(); PRINTF("elseif ");
				print_expr(it.cond); print_newline();
				print_stmt_block(it.block);
			}
			if (!s->if_stmt.else_block.stmts.empty())
			{
				print_newline(); PRINTF("else ");
				print_newline();
				print_stmt_block(s->if_stmt.else_block);
			}
			--indent;
			PRINTF(")"); break;
		case Stmt::WHILE:
			PRINTF("(while "); print_expr(s->while_stmt.cond);
			++indent; print_newline();
			print_stmt_block(s->while_stmt.block); --indent;
			PRINTF(")"); break;
		case Stmt::DO_WHILE:
			PRINTF("(do-while "); print_expr(s->while_stmt.cond);
			++indent; print_newline();
			print_stmt_block(s->while_stmt.block); --indent;
			PRINTF(")"); break;
		case Stmt::FOR:
			PRINTF("(for ");
			print_stmt(s->for_stmt.init);
			print_expr(s->for_stmt.cond);
			print_stmt(s->for_stmt.next);
			++indent;
			print_newline();
			print_stmt_block(s->for_stmt.block);
			--indent;
			PRINTF(")"); break;
		case Stmt::SWITCH:
			PRINTF("(switch ");
			print_expr(s->switch_stmt.expr);
			++indent;
			for (SwitchCase &it : s->switch_stmt.cases)
			{
				print_newline();
				PRINTF("(case (%s", it.is_default ? " default" : "");
				for (Expr *exprIt : it.exprs) {
					PRINTF(" "); print_expr(exprIt);
				}
				PRINTF(" ) ");
				++indent;
				print_newline(); print_stmt_block(it.block);
				--indent;
			}
			--indent;
			PRINTF(")"); break;
		case Stmt::ASSIGN:
			PRINTF("(%s ", token_kind_name(s->assign.op));
			print_expr(s->assign.left);
			if (s->assign.right) {
				PRINTF(" "); print_expr(s->assign.right);
			}
			PRINTF(")"); break;
		case Stmt::INIT:
			PRINTF("(:= %s ", s->init.name);
			print_expr(s->init.expr);
			PRINTF(")"); break;
		case Stmt::EXPR: print_expr(s->expr); break;
		default: assert(0); break;
		}
	}

	void print_aggregate_decl(Decl *decl)
	{
		Decl *d{ decl };
		for (AggregateItem &it : d->aggregate.items)
		{
			print_newline();
			PRINTF("("); print_typespec(it.type);
			for (const char *name : it.names)
			{
				PRINTF(" %s", name);
			}
			PRINTF(")");
		}
	}
	void print_decl(Decl *decl)
	{
		Decl *d = decl;
		switch (d->kind)
		{
		case Decl::ENUM:
			PRINTF("(enum %s", d->name);
			indent++;
			for (EnumItem &it : d->enum_decl.items)
			{
				print_newline();
				PRINTF("(%s ", it.name);
				if (it.init) print_expr(it.init);
				else PRINTF("nil");
				PRINTF(")");
			}
			indent--;
			PRINTF(")");
			break;
		case Decl::STRUCT:
			PRINTF("(struct %s", d->name);
			indent++;
			print_aggregate_decl(d);
			indent--;
			PRINTF(")");
			break;
		case Decl::UNION:
			PRINTF("(union %s", d->name);
			indent++;
			print_aggregate_decl(d);
			indent--;
			PRINTF(")");
			break;
		case Decl::VAR:
			PRINTF("(var %s ", d->name);
			if (d->var.type) print_typespec(d->var.type);
			else PRINTF("nil");
			PRINTF(" ");
			if (d->var.expr) print_expr(d->var.expr);
			else PRINTF("nil");
			PRINTF(")");
			break;
		case Decl::CONST:
			PRINTF("(const %s ", d->name);
			print_expr(d->const_decl.expr);
			PRINTF(")");
			break;
		case Decl::TYPEDEF:
			PRINTF("(typedef %s ", d->name);
			print_typespec(d->typedef_decl.type);
			PRINTF(")");
			break;
		case Decl::FUNC:
			PRINTF("(func %s ", d->name);
			PRINTF("(");
			for (FuncParam &it : d->func.params)
			{
				PRINTF(" %s ", it.name);
				print_typespec(it.type);
			}
			PRINTF(" ) ");
			if (d->func.ret_type) print_typespec(d->func.ret_type);
			else PRINTF("nil");
			indent++;
			print_newline();
			print_stmt_block(d->func.block);
			indent--;
			PRINTF(")");
			break;
		default: assert(0); break;
		}
	}
	void print_test()
	{
		std::vector<Expr*> exprs = {
			expr_binary(token_cast('+'), expr_int(1), expr_int(2)),
			expr_unary(token_cast('-'), expr_float(3.14)),
			expr_ternary(expr_name("flag"), expr_str("true"), expr_str("false")),
			expr_field(expr_name("person"), "name"),
			expr_call(expr_name("fact"), std::vector<Expr*> { expr_int(42) }),
			expr_index(expr_field(expr_name("person"), "siblings"), expr_int(3)),
			expr_cast(typespec_ptr(typespec_name("int")), expr_name("void_ptr"))
		};
		for (Expr *it : exprs)
		{
			print_expr(it); PRINTF("\n");
		}

		std::vector<Stmt *> stmts = {
			stmt_return(expr_int(42)), stmt_break(), stmt_continue(),
			stmt_block(StmtList{ std::vector<Stmt*> { stmt_break(), stmt_continue() }, }),
			stmt_expr(expr_call(expr_name("print"), std::vector<Expr*> { expr_int(1), expr_int(2) })),
			stmt_init("x", expr_int(42)), stmt_if(expr_name("flag1"), StmtList{ std::vector<Stmt*> {
			stmt_return(expr_int(1)) }, },
				std::vector<ElseIf> { {expr_name("flag2"),
				StmtList{ std::vector<Stmt*> { stmt_return(expr_int(2)) } } } },
				StmtList{ std::vector<Stmt*> { stmt_return(expr_int(3)) } }
				),
			stmt_while(expr_name("running"),
				StmtList{ std::vector<Stmt*> {
				stmt_assign(Token::ADD_ASSIGN, expr_name("i"), expr_int(16)) } }),
				stmt_switch(expr_name("val"), std::vector<SwitchCase> { { {
						std::vector<Expr*> { expr_int(3), expr_int(4) },
							false, StmtList{ std::vector<Stmt*> { stmt_return(expr_name("val")) }, }, },
						{ std::vector<Expr*>{expr_int(1)}, true, StmtList{ std::vector<Stmt*> {
							stmt_return(expr_int(0)) } } } } })
		};
		for (Stmt *it : stmts)
		{
			print_stmt(it); PRINTF("\n");
		}
	}
}