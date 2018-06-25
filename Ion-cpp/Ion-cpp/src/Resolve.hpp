#pragma once
#include "stdafx.h"
#include "Parse.hpp"

// TODO: add compiler warnings in case of wrong ordering for a cleaner coding style

namespace Ion
{
	struct TypeField;
	struct Entity;

	struct Type
	{
		enum Kind
		{
			NONE,
			INCOMPLETE, COMPLETING,
			INT, FLOAT, PTR,
			ARRAY, STRUCT, UNION,
			ENUM, FUNC,
		} kind;
		size_t size;
		Entity *entity;
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
		Type(Type::Kind k, size_t s) : kind(k), size(s) {}
		~Type() { }
	};
	struct TypeField
	{
		const char *name;
		Type *type;
	};


	Type *type_alloc(Type::Kind kind);

	extern Type type_int_val;
	extern Type type_float_val;
	extern Type *type_int;
	extern Type *type_float;
	extern const size_t PTR_SIZE;

	size_t type_sizeof(Type *type);

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

	Type *type_struct(std::vector<TypeField> fields);
	Type *type_union(std::vector<TypeField> fields);

	struct CachedFuncType
	{
		std::vector<Type*> params;
		Type *ret;
		Type *func;
	};
	extern std::vector<CachedFuncType> cached_func_types;
	Type *type_func(std::vector<Type*> params, Type *ret);

	void type_complete_struct(Type *type, std::vector<TypeField> fields);
	void type_complete_union(Type *type, std::vector<TypeField> fields);
	Type *type_incomplete(Entity *entity);

	struct Entity
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

	extern std::vector<Entity*> entities;
	Entity *entity_new(Entity::Kind kind, const char *name, Decl *decl);
	Entity *entity_decl(Decl *decl);
	Entity *entity_enum_const(const char *name, Decl *decl);
	Entity *entity_get(const char *name);
	Entity *entity_install_decl(Decl *decl);
	Entity *entity_install_type(const char *name, Type *type);

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

	Entity *resolve_name(const char *name);
	int64_t resolve_int_const_expr(Expr *expr);
	ResolvedExpr resolve_expr(Expr *expr);

	Type *resolve_typespec(Typespec *typespec);

	extern std::vector<Entity*> ordered_entities;
	void complete_type(Type *type);

	Type *resolve_decl_type(Decl *decl);
	Type *resolve_decl_var(Decl *decl);
	Type *resolve_decl_const(Decl *decl, int64_t *val);
	void resolve_entity(Entity *entity);
	void complete_entity(Entity *entity);

	Entity *resolve_name(const char *name);
	ResolvedExpr resolve_expr_field(Expr *expr);
	ResolvedExpr resolve_expr_name(Expr *expr);
	ResolvedExpr resolve_expr_unary(Expr *expr);
	ResolvedExpr resolve_expr_binary(Expr *expr);
	ResolvedExpr resolve_expr_compound(Expr *expr);
	ResolvedExpr resolve_expr(Expr *expr);
	int64_t resolve_int_const_expr(Expr *expr);
	void resolve_test();
}