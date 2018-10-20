#pragma once
#include "stdafx.h"
#include "Parse.hpp"

namespace Ion
{
	struct TypeField;
	struct Sym;

	struct Type
	{
		enum Kind
		{
			NONE,
			INCOMPLETE, COMPLETING,
			VOID, CHAR,
			INT, FLOAT, PTR,
			ARRAY, STRUCT, UNION,
			ENUM, FUNC,
		} kind;
		size_t size;
		size_t align;
		Sym *sym;
		union
		{
			struct
			{
				Type *elem;
			} ptr;
			struct
			{
				Type *elem;
				size_t size;
			} array;
			struct
			{
				std::vector<TypeField> fields;
			} aggregate;
			struct
			{
				std::vector<Type*> params;
				Type *ret;
			} func;
		};
		Type(Type::Kind k, size_t s, size_t a) : kind(k), size(s), align(a) {}
		~Type() { }
	};
	struct TypeField
	{
		const char *name;
		Type *type;
	};


	Type *type_alloc(Type::Kind kind);

	extern Type type_void_val;
	extern Type type_char_val;
	extern Type type_int_val;
	extern Type type_float_val;

	extern Type *type_void;
	extern Type *type_char;
	extern Type *type_int;
	extern Type *type_float;
	extern const size_t PTR_SIZE;
	extern const size_t PTR_ALIGN;

	size_t type_sizeof(Type *type);
	size_t type_alignof(Type *type);

	struct CachedPtrType
	{
		Type *elem;
		Type *ptr;
	};
	extern std::vector<CachedPtrType> cached_ptr_types;
	Type *type_ptr(Type *elem);

	struct CachedArrayType
	{
		Type *elem;
		size_t size;
		Type *array;
	};
	extern std::vector<CachedArrayType> cached_array_types;
	Type *type_array(Type *elem, size_t size);

	struct CachedFuncType
	{
		std::vector<Type*> params;
		Type *ret;
		Type *func;
	};
	extern std::vector<CachedFuncType> cached_func_types;
	Type *type_func(std::vector<Type*> params, Type *ret);

	bool duplicate_fields(std::vector<TypeField> fields);
	void type_complete_struct(Type *type, std::vector<TypeField> fields);
	void type_complete_union(Type *type, std::vector<TypeField> fields);
	Type *type_incomplete(Sym *sym);

	struct Sym
	{
		const char *name;
		enum Kind
		{
			NONE, VAR,
			CONST, FUNC,
			TYPE, ENUM_CONST,
		} kind;
		enum State
		{
			UNRESOLVED,
			RESOLVING,
			RESOLVED
		} state;
		Decl *decl;
		Type *type;
		int64_t val;
	};

	enum
	{
		MAX_LOCAL_SYMS = 1024,
	};
	extern std::vector<Sym*> global_syms;
	extern Sym* local_syms[MAX_LOCAL_SYMS];
	extern Sym **local_syms_end;
	Sym *sym_new(Sym::Kind kind, const char *name, Decl *decl);
	Sym *sym_decl(Decl *decl);
	Sym *sym_enum_const(const char *name, Decl *decl);
	Sym *sym_get(const char *name);
	Sym *sym_install_decl(Decl *decl);
	Sym *sym_install_type(const char *name, Type *type);

	struct ResolvedExpr
	{
		Type *type;
		bool is_lvalue;
		bool is_const;
		int64_t val;
	};
	extern ResolvedExpr resolved_null;

	inline ResolvedExpr resolved_rvalue(Type *type) {
		return ResolvedExpr{ type };
	}

	inline ResolvedExpr resolved_lvalue(Type *type) {
		return ResolvedExpr{ type, true };
	}

	inline ResolvedExpr resolved_const(int64_t val) {
		return ResolvedExpr{ type_int, false, true, val };
	}

	Sym *resolve_name(const char *name);
	int64_t resolve_const_expr(Expr *expr);
	ResolvedExpr resolve_expr(Expr *expr, Type *expected_type = nullptr);

	Type *resolve_typespec(Typespec *typespec);

	extern std::vector<Sym*> ordered_syms;
	void complete_type(Type *type);

	Type *resolve_decl_type(Decl *decl);
	Type *resolve_decl_var(Decl *decl);
	Type *resolve_decl_const(Decl *decl, int64_t *val);
	Type *resolve_decl_func(Decl *decl);
	void resolve_sym(Sym *sym);
	void complete_sym(Sym *sym);

	Sym *resolve_name(const char *name);
	ResolvedExpr resolve_expr_field(Expr *expr);
	ResolvedExpr ptr_decay(ResolvedExpr expr);
	ResolvedExpr resolve_expr_name(Expr *expr);
	int64_t eval_int_unary(Token::Kind op, int64_t val);
	int64_t eval_int_binary(Token::Kind op, int64_t left, int64_t right);
	ResolvedExpr resolve_expr_unary(Expr *expr);
	ResolvedExpr resolve_expr_binary(Expr *expr);
	ResolvedExpr resolve_expr_compound(Expr *expr, Type *expected_type);
	ResolvedExpr resolve_expr_call(Expr *expr);
	ResolvedExpr resolve_expr_ternary(Expr *expr, Type *expected_type);
	ResolvedExpr resolve_expr_index(Expr *expr);
	ResolvedExpr resolve_expr(Expr *expr, Type *expected_type);
	int64_t resolve_const_expr(Expr *expr);
	void resolve_test();
}