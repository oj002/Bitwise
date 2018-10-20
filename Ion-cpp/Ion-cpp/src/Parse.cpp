#include "Parse.hpp"

namespace Ion
{
	Typespec *parse_type_func()
	{
		std::vector<Typespec *> args;
		expect_token(Token::LPAREN);
		if (!is_token(Token::RPAREN))
		{
			args.push_back(parse_type());
			while (match_token(Token::COMMA))
				args.push_back(parse_type());
		}
		expect_token(Token::LPAREN);
		Typespec *ret{ nullptr };
		if (match_token(Token::COLON))
			ret = parse_type();
		return typespec_func(args, ret);
	}
	Typespec *parse_type_base()
	{
		if (is_token(Token::NAME))
		{
			const char *name{ token.name };
			next_token();
			return typespec_name(name);
		}
		else if (match_keyword(func_keyword))
			return parse_type_func();
		else if (match_token(Token::LPAREN))
		{
			Typespec *type{ parse_type() };
			expect_token(Token::RPAREN);
			return type;
		}
		else
		{
			fatal_syntax_error("Unexpected token %s in type", token_info());
			return nullptr;
		}
	}
	Typespec *parse_type()
	{
		Typespec *type{ parse_type_base() };
		while (is_token(Token::LBRACKET) || is_token(Token::MUL))
		{
			if (match_token(Token::LBRACKET))
			{
				Expr *expr{ nullptr };
				if (!is_token(Token::RBRACKET)) expr = parse_expr();
				expect_token(Token::RBRACKET);
				type = typespec_array(type, expr);
			}
			else
			{
				assert(is_token(Token::MUL));
				next_token();
				type = typespec_ptr(type);
			}
		}
		return type;
	}

	CompoundField parse_expr_compound_field()
	{
		if (match_token(Token::LBRACKET))
		{
			Expr *index = parse_expr();
			expect_token(Token::RBRACKET);
			expect_token(Token::ASSIGN);
			CompoundField f;
			f.kind = CompoundField::INDEX;
			f.init = parse_expr();
			f.index = index;
			return f;
		}
		else
		{
			Expr *expr = parse_expr();
			if (match_token(Token::ASSIGN))
			{
				if (expr->kind != Expr::NAME) 
					fatal_syntax_error("Named initializer in compound literal must be preceded by field name");
				CompoundField f;
				f.kind = CompoundField::NAME;
				f.init = parse_expr();
				f.name = expr->name;
				return f;
			}
			else
				return { CompoundField::DEFAULT, expr };
		}
	}

	Expr *parse_expr_compound(Typespec *type)
	{
		expect_token(Token::LBRACE);
		std::vector<CompoundField> fields;
		if (!is_token(Token::RBRACE))
		{
			fields.push_back(parse_expr_compound_field());
			while (match_token(Token::COMMA))
				fields.push_back(parse_expr_compound_field());
		}
		expect_token(Token::RBRACE);
		return expr_compound(type, fields);
	}
	
	Expr *parse_expr_operand()
	{
		if (is_token(Token::INT))
		{
			uint64_t val{ token.int_val };
			next_token();
			return expr_int(val);
		}
		else if (is_token(Token::FLOAT))
		{
			double val{ token.float_val };
			next_token();
			return expr_float(val);
		}
		else if (is_token(Token::STR))
		{
			const char *val{ token.str_val };
			next_token();
			return expr_str(val);
		}
		else if (is_token(Token::NAME))
		{
			const char *name{ token.name };
			next_token();
			if (is_token(Token::LBRACE)) return parse_expr_compound(typespec_name(name));
			else return expr_name(name);
		}
		else if (match_keyword(cast_keyword))
		{
			expect_token(Token::LPAREN);
			Typespec *type = parse_type();
			expect_token(Token::COMMA);
			Expr *expr = parse_expr();
			expect_token(Token::RPAREN);
			return expr_cast(type, expr);
		}
		else if (match_keyword(sizeof_keyword))
		{
			expect_token(Token::LPAREN);
			if (match_token(Token::COLON))
			{
				Typespec *type{ parse_type() };
				expect_token(Token::RPAREN);
				return expr_sizeof(type);
			}
			else
			{
				Expr *expr{ parse_expr() };
				expect_token(Token::RPAREN);
				return expr_sizeof(expr);
			}
		}
		else if (is_token(Token::LBRACE)) { return parse_expr_compound(nullptr); }
		else if (match_token(Token::LPAREN))
		{
			if (match_token(Token::COLON))
			{
				Typespec *type{ parse_type() };
				expect_token(Token::RPAREN);
				return parse_expr_compound(type);
			}
			else
			{
				Expr *expr{ parse_expr() };
				expect_token(Token::RPAREN);
				return expr;
			}
		}
		else
		{
			fatal_syntax_error("Unexpected token %s in expression", token_kind_name(token.kind)); return nullptr;
		}
	}
	Expr *parse_expr_base()
	{
		Expr *expr{ parse_expr_operand() };
		while (is_token(Token::LPAREN) || is_token(Token::LBRACKET) || is_token(Token::DOT))
		{
			if (match_token(Token::LPAREN))
			{
				std::vector<Expr*> args;
				if (!is_token(Token::RPAREN))
				{
					args.push_back(parse_expr());
					while (match_token(Token::COMMA))
						args.push_back(parse_expr());
				}
				expect_token(Token::RPAREN);
				expr = expr_call(expr, args);
			}
			else if (match_token(Token::LBRACKET))
			{
				Expr *index{ parse_expr() };
				expect_token(Token::RBRACKET);
				expr = expr_index(expr, index);
			}
			else
			{
				assert(is_token(Token::DOT));
				next_token();
				const char *field{ token.name };
				expect_token(Token::NAME);
				expr = expr_field(expr, field);
			}
		}
		return expr;
	}
	bool is_unary_op()
	{
		return is_token(Token::ADD) || is_token(Token::SUB) || is_token(Token::MUL) || is_token(Token::AND) || is_token(Token::NEG) || is_token(Token::NOT);
	}
	Expr *parse_expr_unary()
	{
		if (is_unary_op())
		{
			const Token::Kind op{ token.kind };
			next_token();
			return expr_unary(op, parse_expr_unary());
		}
		else return parse_expr_base();
	}
	bool is_mul_op()
	{
		return Token::FIRST_MUL <= token.kind && token.kind <= Token::LAST_MUL;
	}
	Expr *parse_expr_mul()
	{
		Expr *expr{ parse_expr_unary() };
		while (is_mul_op())
		{
			const Token::Kind op{ token.kind };
			next_token();
			expr = expr_binary(op, expr, parse_expr_unary());
		}
		return expr;
	}
	bool is_add_op()
	{
		return Token::FIRST_ADD <= token.kind && token.kind <= Token::LAST_ADD;
	}
	Expr *parse_expr_add()
	{
		Expr *expr{ parse_expr_mul() };
		while (is_add_op())
		{
			const Token::Kind op{ token.kind };
			next_token();
			expr = expr_binary(op, expr, parse_expr_mul());
		}
		return expr;
	}
	bool is_cmp_op()
	{
		return Token::FIRST_CMP <= token.kind && token.kind <= Token::LAST_CMP;
	}
	Expr *parse_expr_cmp()
	{
		Expr *expr{ parse_expr_add() };
		while (is_cmp_op())
		{
			const Token::Kind op{ token.kind };
			next_token();
			expr = expr_binary(op, expr, parse_expr_add());
		}
		return expr;
	}
	Expr *parse_expr_and()
	{
		Expr *expr{ parse_expr_cmp() };
		while (match_token(Token::AND))
			expr = expr_binary(Token::AND, expr, parse_expr_cmp());
		return expr;
	}
	Expr *parse_expr_or()
	{
		Expr *expr{ parse_expr_and() };
		while (match_token(Token::OR))
			expr = expr_binary(Token::OR, expr, parse_expr_and());
		return expr;
	}
	Expr *parse_expr_ternary()
	{
		Expr *expr{ parse_expr_or() };
		if (match_token(Token::QUESTION))
		{
			Expr *then_expr{ parse_expr_ternary() };
			expect_token(Token::COLON);
			Expr *else_expr{ parse_expr_ternary() };
			expr = expr_ternary(expr, then_expr, else_expr);
		}
		return expr;
	}
	Expr *parse_expr() { return parse_expr_ternary(); }
	Expr *parse_paren_expr()
	{
		expect_token(Token::LPAREN);
		Expr *expr{ parse_expr() };
		expect_token(Token::RPAREN);
		return expr;
	}
	StmtList parse_stmt_block()
	{
		expect_token(Token::LBRACE);
		std::vector<Stmt*> stmts;
		while (!is_token(Token::END_OF_FILE) && !is_token(Token::RBRACE))
			stmts.push_back(parse_stmt());
		expect_token(Token::RBRACE);
		return StmtList{ stmts };
	}
	Stmt *parse_stmt_if()
	{
		Expr *cond{ parse_paren_expr() };
		StmtList then_block{ parse_stmt_block() };
		StmtList else_block{ { 0 } };
		std::vector<ElseIf> elseifs;
		while (match_keyword(else_keyword))
		{
			if (!match_keyword(if_keyword))
			{
				else_block = parse_stmt_block();
				break;
			}
			Expr *elseif_cond = parse_paren_expr();
			StmtList elseif_block = parse_stmt_block();
			elseifs.push_back({ elseif_cond, elseif_block });
		}
		return stmt_if(cond, then_block, elseifs, else_block);
	}

	Stmt *parse_stmt_while()
	{
		Expr *cond{ parse_paren_expr() };
		return stmt_while(cond, parse_stmt_block());
	}

	Stmt *parse_stmt_do_while()
	{
		StmtList block{ parse_stmt_block() };
		if (!match_keyword(while_keyword))
		{
			fatal_syntax_error("Expected 'while' after 'do' block");
			return nullptr;
		}
		Stmt *stmt{ stmt_do_while(parse_paren_expr(), block) };
		expect_token(Token::SEMICOLON);
		return stmt;
	}
	bool is_assign_op()
	{
		return Token::FIRST_ASSIGN <= token.kind && token.kind <= Token::LAST_ASSIGN;
	}
	Stmt *parse_simple_stmt()
	{
		Expr *expr{ parse_expr() };
		Stmt *stmt;
		if (match_token(Token::COLON_ASSIGN))
		{
			if (expr->kind != Expr::NAME) fatal_syntax_error(":= must be preceded by a name");
			stmt = stmt_init(expr->name, parse_expr());
		}
		else if (is_assign_op())
		{
			const Token::Kind op{ token.kind };
			next_token();
			stmt = stmt_assign(op, expr, parse_expr());
		}
		else if (is_token(Token::INC) || is_token(Token::DEC))
		{
			const Token::Kind op{ token.kind };
			next_token();
			stmt = stmt_assign(op, expr, nullptr);
		}
		else stmt = stmt_expr(expr);
		return stmt;
	}
	Stmt *parse_stmt_for()
	{
		expect_token(Token::LPAREN);
		Stmt *init{ nullptr };
		if (!is_token(Token::SEMICOLON)) init = parse_simple_stmt();
		expect_token(Token::SEMICOLON);
		Expr *cond{ nullptr };
		if (!is_token(Token::SEMICOLON)) cond = parse_expr();
		expect_token(Token::SEMICOLON);
		Stmt *next{ nullptr };
		if (!is_token(Token::RPAREN))
		{
			next = parse_simple_stmt();
			if (next->kind == Stmt::INIT)
				syntax_error("Init statements not allowed in for-statement's next clause");
		}
		expect_token(Token::RPAREN);
		return stmt_for(init, cond, next, parse_stmt_block());
	}
	SwitchCase parse_stmt_switch_case()
	{
		std::vector<Expr*>exprs;
		bool is_default{ false };
		while (is_keyword(case_keyword) || is_keyword(default_keyword))
		{
			if (match_keyword(case_keyword))
				exprs.push_back(parse_expr());
			else
			{
				assert(is_keyword(default_keyword));
				next_token();
				if (is_default)
					syntax_error("Duplicate default labels in same switch clause");
				is_default = true;
			}
			expect_token(Token::COLON);
		}
		std::vector<Stmt*>stmts;
		while (!is_token(Token::END_OF_FILE) && !is_token(Token::RBRACE) && !is_keyword(case_keyword) && !is_keyword(default_keyword))
			stmts.push_back(parse_stmt());
		
		return SwitchCase{ exprs, is_default, { stmts } };
	}
	Stmt *parse_stmt_switch()
	{
		Expr *expr{ parse_paren_expr() };
		std::vector<SwitchCase> cases;
		expect_token(Token::LBRACE);
		while (!is_token(Token::END_OF_FILE) && !is_token(Token::RBRACE))
			cases.push_back(parse_stmt_switch_case());
		expect_token(Token::RBRACE);
		return stmt_switch(expr, cases);
	}
	Stmt *parse_stmt()
	{
		if (match_keyword(if_keyword))
			return parse_stmt_if();
		else if (match_keyword(while_keyword))
			return parse_stmt_while();
		else if (match_keyword(do_keyword))
			return parse_stmt_do_while();
		else if (match_keyword(for_keyword))
			return parse_stmt_for();
		else if (match_keyword(switch_keyword))
			return parse_stmt_switch();
		else if (is_token(Token::LBRACE))
			return stmt_block(parse_stmt_block());
		else if (match_keyword(break_keyword))
		{
			expect_token(Token::SEMICOLON);
			return stmt_break();
		}
		else if (match_keyword(continue_keyword))
		{
			expect_token(Token::SEMICOLON);
			return stmt_continue();
		}
		else if (match_keyword(return_keyword))
		{
			Expr *expr = NULL;
			if (!is_token(Token::SEMICOLON))
				expr = parse_expr();
			expect_token(Token::SEMICOLON);
			return stmt_return(expr);
		}
		else
		{
			Decl *decl = parse_decl_opt();
			if (decl) return stmt_decl(decl);
			Stmt *stmt = parse_simple_stmt();
			expect_token(Token::SEMICOLON);
			return stmt;
		}
	}

	const char *parse_name()
	{
		const char *name{ token.name };
		expect_token(Token::NAME);
		return name;
	}
	EnumItem parse_decl_enum_item()
	{
		const char *name{ parse_name() };
		Expr *init{ nullptr };
		if (match_token(Token::ASSIGN))
			init = parse_expr();
		return { name, init };
	}
	Decl *parse_decl_enum()
	{
		const char *name{ parse_name() };
		expect_token(Token::LBRACE);
		std::vector<EnumItem> items;
		if (!is_token(Token::LBRACE))
		{
			items.push_back(parse_decl_enum_item());
			while (match_token(Token::COMMA))
				items.push_back(parse_decl_enum_item());
		}
		expect_token(Token::RBRACE);
		return decl_enum(name, items);
	}
	AggregateItem parse_decl_aggregate_item()
	{
		std::vector<const char *> names;
		names.push_back(parse_name());
		while (match_token(Token::COMMA))
			names.push_back(parse_name());
		expect_token(Token::COLON);
		Typespec *type{ parse_type() };
		expect_token(Token::SEMICOLON);
		return AggregateItem{ names, type };
	}
	Decl *parse_decl_aggregate(Decl::Kind kind)
	{
		assert(kind == Decl::STRUCT || kind == Decl::UNION);
		const char *name{ parse_name() };
		expect_token(Token::LBRACE);
		std::vector<AggregateItem> items;
		while (!is_token(Token::END_OF_FILE) && !is_token(Token::RBRACE))
			items.push_back(parse_decl_aggregate_item());
		expect_token(Token::RBRACE);
		return decl_aggregate(kind, name, items);
	}
	Decl *parse_decl_var()
	{
		const char *name{ parse_name() };
		if (match_token(Token::ASSIGN)) 
			return decl_var(name, nullptr, parse_expr());
		else if (match_token(Token::COLON))
		{
			Typespec *type{ parse_type() };
			Expr *expr{ nullptr };
			if (match_token(Token::ASSIGN))
				expr = parse_expr();
			return decl_var(name, type, expr);
		}
		else
		{
			fatal_syntax_error("Expected : or = after var, got %s", token_kind_name(token.kind));
			return nullptr;
		}
	}
	Decl *parse_decl_const()
	{
		const char *name{ parse_name() };
		expect_token(Token::ASSIGN);
		return decl_const(name, parse_expr());
	}
	Decl *parse_decl_typedef()
	{
		const char *name{ parse_name() };
		expect_token(Token::ASSIGN);
		return decl_typedef(name, parse_type());
	}
	FuncParam parse_decl_func_param()
	{
		const char *name{ parse_name() };
		expect_token(Token::COLON);
		Typespec *type{ parse_type() };
		return FuncParam{ name, type };
	}

	Decl *parse_decl_func()
	{
		const char *name{ parse_name() };
		expect_token(Token::LPAREN);
		std::vector<FuncParam> params;
		if (!is_token(Token::RPAREN))
		{
			params.push_back(parse_decl_func_param());
			while (match_token(Token::COMMA))
				params.push_back(parse_decl_func_param());
		}
		expect_token(Token::RPAREN);
		Typespec *ret_type{ nullptr };
		if (match_token(Token::COLON))
			ret_type = parse_type();
		StmtList block{ parse_stmt_block() };
		return decl_func(name, params, ret_type, block);
	}

	Decl *parse_decl_opt()
	{
		if (match_keyword(enum_keyword)) { return parse_decl_enum(); }
		else if (match_keyword(struct_keyword)) { return parse_decl_aggregate(Decl::STRUCT); }
		else if (match_keyword(union_keyword)) { return parse_decl_aggregate(Decl::UNION); }
		else if (match_keyword(var_keyword)) { return parse_decl_var();}
		else if (match_keyword(const_keyword)) { return parse_decl_const(); }
		else if (match_keyword(typedef_keyword)) { return parse_decl_typedef(); }
		else if (match_keyword(func_keyword)) { return parse_decl_func(); }
		else
		{
			fatal_syntax_error("Expected declaration keyword, got %s", token_kind_name(token.kind));
			return nullptr;
		}
	}

	Decl *parse_decl()
	{
		Decl *decl{ parse_decl_opt() };
		if (!decl)
			fatal_syntax_error("Expected declaration keyword, got %s", token_info());
		return decl;
	}

	void parse_and_print_decl(const char *str)
	{
		init_stream(str);
		Decl *decl{ parse_decl() };
		print_decl(decl);
		std::printf("\n");
	}

	void parse_test()
	{
		parse_and_print_decl("const n = sizeof(1+2)");
		parse_and_print_decl("const n = sizeof(:int*[16])");
		parse_and_print_decl("func fact(n: int): int { trace(\"fact\"); if (n == 0) { return 1; } else { return n * fact(n-1); } }");
		parse_and_print_decl("func fact(n: int): int { p := 1; for (i := 1; i <= n; i++) { p *= i; } return p; }");
		parse_and_print_decl("var x = b == 1 ? 1+2 : 3-4");
		parse_and_print_decl("const pi = 3.14");
		parse_and_print_decl("struct Vector { x, y: float; }");
		parse_and_print_decl("union IntOrFloat { i: int; f: float; }");
		parse_and_print_decl("typedef Vectors = Vector[1+2]");
	}
}