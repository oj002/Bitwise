#include "Resolve.hpp"

namespace Ion
{
	Type *type_alloc(Type::Kind kind)
	{
		Type *t{ reinterpret_cast<Type *>(xcalloc(1, sizeof(Type))) };
		t->kind = kind;
		return t;
	}

	Type type_int_val{ Type::INT };
	Type type_float_val{ Type::FLOAT };
	Type *type_int{ &type_int_val };
	Type *type_float{ &type_float_val };

	std::vector<CachedPtrType> cached_ptr_types;
	Type *type_ptr(Type *base)
	{
		for (CachedPtrType &it : cached_ptr_types)
		{
			if (it.base == base)
			{
				return it.ptr;
			}
		}
		Type *t{ type_alloc(Type::PTR) };
		t->ptr.base = base;
		cached_ptr_types.push_back({ base, t });
		return t;
	}
	std::vector<CachedArrayType> cached_array_types;
	Type *type_array(Type *base, size_t size)
	{
		for (CachedArrayType &it : cached_array_types)
		{
			if (it.base == base && it.size == size)
			{
				return it.array;
			}
		}
		Type *t{ type_alloc(Type::ARRAY) };
		t->array.base = base;
		t->array.size = size;
		cached_array_types.push_back({ base, size, t });
		return t;
	}
	Type *type_struct(std::vector<TypeField> fields)
	{
		Type *t{ type_alloc(Type::STRUCT) };
		t->aggregate.fields = fields;
		return t;
	}
	Type *type_union(std::vector<TypeField> fields)
	{
		Type *t{ type_alloc(Type::UNION) };
		t->aggregate.fields = fields;
		return t;
	}
	std::vector<CachedFuncType> cached_func_types;
	Type *type_func(std::vector<Type*> params, Type *ret)
	{
		for (CachedFuncType &it : cached_func_types)
		{
			if (it.params == params && it.ret == ret && it.params == params)
			{
				return it.func;
			}
		}
		Type *t{ type_alloc(Type::FUNC) };
		t->func.params = params;
		t->func.ret = ret;
		cached_func_types.push_back({ params, ret, t });
		return t;
	}
	std::vector<Sym> sym_list;
	void sym_put(Decl * decl)
	{
		assert(decl->name);
		assert(!sym_get(decl->name));
		sym_list.push_back({ decl->name, decl, Sym::UNORDERED });
	}
	Sym *sym_get(const char *name)
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
	void order_name(const char *name)
	{
		Sym *sym{ sym_get(name) };
		if (!sym)
		{
			fatal("Non-existent name '%s'", name); return;
		}
		switch (sym->state)
		{
		case Sym::ORDERING: fatal("Cyclic dependency"); return;
		case Sym::UNORDERED:
			sym->state = Sym::ORDERING;
			order_decl(sym->decl);
			sym->state = Sym::ORDERED;
			ordered_decls.push_back(sym->decl);
			return;
		}
	}
	void order_expr(Expr *expr)
	{
		switch (expr->kind)
		{
		case Expr::INT: case Expr::FLOAT: case Expr::STR:
			// Do nothing
			break;
		case Expr::NAME: order_name(expr->name); break;
		case Expr::CAST:
			order_typespec(expr->cast.type);
			order_expr(expr->cast.expr);
			break;
		case Expr::CALL:
			order_expr(expr->call.expr);
			for (Expr *it : expr->call.args)
			{
				order_expr(it);
			}
			break;
		case Expr::INDEX:
			order_expr(expr->index.expr);
			order_expr(expr->index.index);
			break;
		case Expr::FIELD:
			order_expr(expr->field.expr);
			break;
		case Expr::COMPOUND:
			if (expr->compound.type)
			{
				order_typespec(expr->compound.type);
			}
			for (Expr *it : expr->compound.args)
			{
				order_expr(it);
			}
		case Expr::UNARY: order_expr(expr->unary.expr); break;
		case Expr::BINARY:
			order_expr(expr->binary.right);
			order_expr(expr->binary.left);
			break;
		case Expr::TERNARY:
			order_expr(expr->ternary.cond);
			order_expr(expr->ternary.then_expr);
			order_expr(expr->ternary.else_expr);
			break;
		case Expr::SIZEOF_EXPR: order_expr(expr->sizeof_expr); break;
		case Expr::SIZEOF_TYPE: order_typespec(expr->sizeof_type); break;
		default: assert(0); break;
		}

	}
	void order_typespec(Typespec *typespec)
	{
		switch (typespec->kind)
		{
		case Typespec::NAME: order_name(typespec->name); break;
		case Typespec::FUNC:
			for (Typespec *it : typespec->func.args)
			{
				order_typespec(it);
			}
			order_typespec(typespec->func.ret);
			break;
		case Typespec::ARRAY:
			order_typespec(typespec->array.elem);
			order_expr(typespec->array.size);
			break;
		case Typespec::PTR: order_typespec(typespec->ptr.elem); break; // TODO: think about it
		default: assert(0); break;
		}
	}
	void order_decl(Decl *decl)
	{
		switch (decl->kind)
		{
		case Decl::STRUCT: case Decl::UNION:
			for (AggregateItem &it : decl->aggregate.items)
			{
				order_typespec(it.type);
			}
			break;
		case Decl::VAR:
			order_typespec(decl->var.type);
			order_expr(decl->var.expr); break;
		case Decl::CONST: order_expr(decl->const_decl.expr); break;
		case Decl::TYPEDEF: order_typespec(decl->typedef_decl.type); break;
		case Decl::FUNC: break; // Do nothing
		default: assert(0); break;
		}
	}
	void order_decls()
	{
		for (Sym &sym : sym_list)
		{
			order_name(sym.name);
		}
	}
	void resolve_test()
	{
		const char *foo{ str_intern("foo") };
		assert(sym_get(foo) == NULL);
		Decl *decl = decl_const(foo, expr_int(42));
		sym_put(decl);
		Sym *sym = sym_get(foo);
		assert(sym && sym->decl == decl);

		Type *int_ptr = type_ptr(type_int);
		assert(type_ptr(type_int) == int_ptr);
		Type *float_ptr = type_ptr(type_float);
		assert(type_ptr(type_float) == float_ptr);
		assert(int_ptr != float_ptr);
		Type *int_ptr_ptr = type_ptr(type_ptr(type_int));
		assert(type_ptr(type_ptr(type_int)) == int_ptr_ptr);
		Type *float4_array = type_array(type_float, 4);
		assert(type_array(type_float, 4) == float4_array);
		Type *float3_array = type_array(type_float, 3);
		assert(type_array(type_float, 3) == float3_array);
		assert(float4_array != float3_array);
		Type *int_int_func{ type_func(std::vector<Type*>(1, type_int), type_int) };
		assert(type_func(std::vector<Type*>(1, type_int), type_int) == int_int_func);
		Type *int_func = type_func(std::vector<Type*>(), type_int);
		assert(int_int_func != int_func);
		assert(int_func == type_func(std::vector<Type*>(), type_int));

		const char *code[] = {
			"const a = b",
			"const b = 1",
		};
		for (size_t i{ 0 }; i < sizeof(code) / sizeof(*code); ++i)
		{
			init_stream(code[i]);
			Decl *decl{ parse_decl() };
			sym_put(decl);
		}
		order_decls();

		for (Decl *it : ordered_decls)
		{
			printf("%s\n", it->name);
		}
	}
}
