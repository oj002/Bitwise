#pragma once
#include "Lex.hpp"
// TODO: remove malloc stuff and add unique_ptr
namespace Ion
{
	struct Expr;
	struct Stmt;
	struct Decl;
	struct Typespec;

	struct StmtList
	{
		std::vector<Stmt *> stmts;
	};

	struct Typespec
	{
		enum Kind
		{
			NONE, NAME,
			FUNC, ARRAY, 
			PTR
		} kind;

		union
		{
			const char *name;
			struct
			{
				std::vector<Typespec *> args;
				Typespec *ret;
			} func;
			struct
			{
				Typespec *elem;
				Expr *size;
			} array;
			struct
			{
				Typespec *elem;
			} ptr;
		};
	};

	struct FuncParam
	{
		const char * name;
		Typespec *type;
	};
	struct AggregateItem
	{
		std::vector<const char *> names;
		Typespec *type;
	};
	struct EnumItem
	{
		const char *name;
		Expr *init;
	};
	struct Decl
	{
		enum Kind
		{
			NONE, ENUM,
			STRUCT, UNION,
			VAR, CONST,
			TYPEDEF, FUNC
		} kind;
		const char *name;

		union
		{
			struct
			{
				std::vector<EnumItem> items;
			} enum_decl;
			struct
			{
				std::vector<AggregateItem> items;
			} aggregate;
			struct
			{
				std::vector<FuncParam> params;
				Typespec *ret_type;
				StmtList block;
			} func;
			struct
			{
				Typespec *type;
			} typedef_decl;
			struct
			{
				Typespec *type;
				Expr *expr;
			} var;
			struct
			{
				Expr *expr;
			} const_decl;
		};
	};

	struct CompoundField
	{
		enum Kind { DEFAULT, NAME, INDEX } kind;
		Expr *init;
		union
		{
			const char *name;
			Expr *index;
		};
	};
	struct Expr
	{
		enum Kind
		{
			NONE, INT,
			FLOAT, STR,
			NAME, CAST,
			CALL, INDEX,
			FIELD, COMPOUND,
			UNARY, BINARY,
			TERNARY,
			SIZEOF_EXPR, SIZEOF_TYPE
		} kind;

		union
		{
			uint64_t int_val;
			double float_val;
			const char *str_val;
			const char *name;
			Expr *sizeof_expr;
			Typespec *sizeof_type;
			struct
			{
				Typespec *type;
				std::vector<CompoundField> fields;
			} compound;
			struct
			{
				Typespec *type;
				Expr * expr;
			} cast;
			struct
			{
				Token::Kind op;
				Expr *expr;
			} unary;
			struct
			{
				Token::Kind op;
				Expr *left;
				Expr *right;
			} binary;
			struct
			{
				Expr *cond;
				Expr *then_expr;
				Expr *else_expr;
			} ternary;
			struct
			{
				Expr  *expr;
				std::vector<Expr *> args;
			} call;
			struct
			{
				Expr *expr;
				Expr *index;
			} index;
			struct
			{
				Expr *expr;
				const char *name;
			} field;
		};
	};

	struct ElseIf
	{
		Expr *cond;
		StmtList block;
	};
	struct SwitchCase 
	{
		std::vector<Expr *> exprs;
		bool is_default;
		StmtList block;
	};

	struct Stmt
	{
		enum Kind
		{
			NONE, DECL,
			RETURN, BREAK,
			CONTINUE, BLOCK,
			IF, WHILE,
			DO_WHILE, FOR,
			SWITCH, ASSIGN,
			INIT, EXPR
		} kind;
		union
		{
			Expr *expr;
			Decl *decl;
			struct
			{
				Expr *cond;
				StmtList then_block;
				std::vector<ElseIf> elseifs;
				StmtList else_block;
			} if_stmt;
			struct
			{
				Expr *cond;
				StmtList block;
			} while_stmt;
			struct
			{
				Stmt *init;
				Expr *cond;
				Stmt *next;
				StmtList block;
			} for_stmt;
			struct
			{
				Expr *expr;
				std::vector<SwitchCase> cases;
			} switch_stmt;
			StmtList block;
			struct
			{
				Token::Kind op;
				Expr *left;
				Expr *right;
			} assign;
			struct
			{
				const char *name;
				Expr *expr;
			} init;
		};
	};

	void *ast_alloc(size_t size);
	void *ast_dup(const void *src, size_t size);
	Typespec *typespec_new(Typespec::Kind kind);

	Typespec *typespec_name(const char *name);
	Typespec *typespec_ptr(Typespec *elem);
	Typespec *typespec_array(Typespec *elem, Expr *size);
	Typespec *typespec_func(std::vector<Typespec *> args, Typespec *ret);

	Decl *decl_new(Decl::Kind kind, const char *name);
	Decl *decl_enum(const char *name, std::vector<EnumItem> items);
	Decl *decl_aggregate(Decl::Kind kind, const char *name, std::vector<AggregateItem> items);
	Decl *decl_union(const char *name, std::vector<AggregateItem> items);
	Decl *decl_var(const char *name, Typespec *type, Expr *expr);
	Decl *decl_func(const char *name, std::vector<FuncParam> params, Typespec *ret_type, StmtList block);
	Decl *decl_const(const char *name, Expr *expr);
	Decl *decl_typedef(const char *name, Typespec *type);

	Expr *expr_new(Expr::Kind kind);
	Expr *expr_sizeof(Expr *expr);
	Expr *expr_sizeof(Typespec *type);
	Expr *expr_int(uint64_t val);
	Expr *expr_float(double val);
	Expr *expr_str(const char *str);
	Expr *expr_name(const char *name);
	Expr *expr_compound(Typespec *type, std::vector<CompoundField> fields);
	Expr *expr_cast(Typespec *type, Expr *expr);
	Expr *expr_call(Expr *expr, std::vector<Expr *> args);
	Expr *expr_index(Expr *expr, Expr *index);
	Expr *expr_field(Expr *expr, const char *name);
	Expr *expr_unary(Token::Kind op, Expr *expr);
	Expr *expr_binary(Token::Kind op, Expr *left, Expr *right);
	Expr *expr_ternary(Expr *cond, Expr *then_expr, Expr *else_expr);

	Stmt *stmt_new(Stmt::Kind kind);
	Stmt *stmt_decl(Decl *decl);
	Stmt *stmt_return(Expr *expr);
	Stmt *stmt_break();
	Stmt *stmt_continue();
	Stmt *stmt_block(StmtList block);
	Stmt *stmt_if(Expr *cond, StmtList then_block, std::vector<ElseIf> elseifs, StmtList else_block);
	Stmt *stmt_while(Expr *cond, StmtList block);
	Stmt *stmt_do_while(Expr *cond, StmtList block);
	Stmt *stmt_for(Stmt *init, Expr *cond, Stmt *next, StmtList block);
	Stmt *stmt_switch(Expr *expr, std::vector<SwitchCase> cases);
	Stmt *stmt_assign(Token::Kind op, Expr *left, Expr *right);
	template<typename T>
	Stmt *stmt_assign(T op, Expr *left, Expr *right);
	Stmt *stmt_init(const char *name, Expr *expr);
	Stmt *stmt_expr(Expr *expr);
}