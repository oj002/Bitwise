#pragma once
#include "stdafx.h"
#include "Parse.hpp"

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

	static Type type_int_val{ {Type::INT} };
	static Type type_float_val{ { Type::FLOAT } };
	static Type *type_int{ &type_int_val };
	static Type *type_float{ &type_float_val };

	struct CachedPtrType
	{
		Type *base;
		Type *ptr;
	};
	static std::vector<CachedPtrType> cached_ptr_types;
	Type *type_ptr(Type *base);

	struct CachedArrayType
	{
		Type *base;
		size_t size;
		Type *array;
	};
	static std::vector<CachedArrayType> cached_array_types;
	Type *type_array(Type *base, size_t size);

	Type *type_struct(std::vector<TypeField> fields);
	Type *type_union(std::vector<TypeField> fields);

	struct CachedFuncType
	{
		std::vector<Type*> params;
		Type *ret;
		Type *func;
	};
	static std::vector<CachedFuncType> cached_func_types;
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
			UNRESOLVED,
			RESOLVING,
			RESOLVED
		} state;
		Entity *ent;
	};

	static std::vector<Sym> sym_list;

	static Sym *sym_get(const char *name)
	{
		for (Sym &it : sym_list)
		{
			if (it.name == name)
			{
				return &it;
			}
		}
		return nullptr;
	}
	static void sym_put(Decl *decl)
	{
		assert(decl->name);
		assert(!sym_get(decl->name));
		sym_list.push_back({ decl->name, decl, Sym::UNRESOLVED });
	}

	static void resolve_decl(Decl *decl)
	{
		switch (decl->kind)
		{
		case Decl::CONST:
		{
			// ConstEntity *const_ent{ resolve_const_expr(decl->const_decl.expr) };
			break;
		}
		}
	}

	static void resolve_sym(Sym *sym)
	{
		switch (sym->state)
		{
		case Sym::RESOLVING: fatal("Cyclic dependency"); return;
		case Sym::RESOLVED: resolve_decl(sym->decl); return;
		}
	}

	static Sym *resolve_name(const char *name)
	{
		Sym *sym{ sym_get(name) };
		if (sym) { resolve_sym(sym); }
		else
		{
			fatal("Unknown name");
			return nullptr;
		}
		resolve_sym(sym);
		return sym;
	}

	static void resolve_syms()
	{
		for (Sym &it : sym_list)
		{
			resolve_sym(&it);
		}
	}

	// https://youtu.be/0WpCnd9E-eg?t=4663

	static void resolve_test()
	{
		const char *foo{ str_intern("foo") };
		assert(sym_get(foo) == nullptr);
		Decl *decl{ decl_const(foo, expr_int(42)) };
		sym_put(decl);
		Sym *sym{ sym_get(foo) };
		assert(sym && sym->decl == decl);

		Type *int_ptr{ type_ptr(type_int) };
		assert(type_ptr(type_int) == int_ptr);
		Type *float_ptr{ type_ptr(type_float) };
		assert(type_ptr(type_float) == float_ptr);
		assert(int_ptr != float_ptr);
		Type *int_ptr_ptr{ type_ptr(type_ptr(type_int)) };
		assert(type_ptr(type_ptr(type_int)) == int_ptr_ptr);

		Type *float4_array{ type_array(type_float, 4) };
		assert(type_array(type_float, 4) == float4_array);
		Type *float3_array{ type_array(type_float, 3) };
		assert(type_array(type_float, 3) == float3_array);
		assert(float4_array != float3_array);

		Type *int_int_func{ type_func(std::vector<Type*>{type_int}, type_int) };
		assert(type_func(std::vector<Type*>{type_int}, type_int) == int_int_func);
		Type *int_func{ type_func(std::vector<Type*>(), type_int) };
		assert(type_func(std::vector<Type*>(), type_int) == int_func);
	}
}