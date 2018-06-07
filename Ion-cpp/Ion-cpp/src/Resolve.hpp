#pragma once
#include "stdafx.h"
#include "Parse.hpp"

// TODO: add compiler warnings in case of wrong ordering for a cleaner coding style

namespace Ion
{
	struct TypeField;

	struct Type
	{
		enum Kind
		{
			INT, FLOAT,
			PTR, ARRAY,
			STRUCT, UNION,
			FUNC
		} kind;
		union
		{
			struct
			{
				Type *base;
			} ptr;
			struct
			{
				Type *base;
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
		Type(Type::Kind k) : kind(k) {}
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

	struct CachedPtrType
	{
		Type *base;
		Type *ptr;
	};
	extern std::vector<CachedPtrType> cached_ptr_types;
	Type *type_ptr(Type *base);

	struct CachedArrayType
	{
		Type *base;
		size_t size;
		Type *array;
	};
	extern std::vector<CachedArrayType> cached_array_types;
	Type *type_array(Type *base, size_t size);

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

	struct ConstEntity
	{
		Type *type;
		union
		{
			uint64_t int_val;
			double float_val;
		};
	};

	struct Entity
	{
		enum
		{

		} kind;
		union
		{
			ConstEntity const_ent;
		};
	};

	struct Sym
	{
		const char *name;
		Decl *decl;

		enum Kind
		{
			UNORDERED,
			ORDERING,
			ORDERED
		} state;
		Entity *ent;
	};

	extern std::vector<Sym> sym_list;
	Sym *sym_get(const char *name);
	void sym_put(Decl *decl);
	
	static std::vector<Decl *> ordered_decls;

	void order_name(const char *name);
	void order_expr(Expr *expr);
	void sym_put(Decl * decl);
	void order_typespec(Typespec *typespec);
	void order_decl(Decl *decl);

	void order_decls();

	void resolve_test();
}