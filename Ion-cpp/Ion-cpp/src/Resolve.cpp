#include "Resolve.hpp"

namespace Ion
{
	Type *type_alloc(Type::Kind kind)
	{
		Type *t{ reinterpret_cast<Type *>(xcalloc(1, sizeof(Type))) };
		t->kind = kind;
		return t;
	}
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
}
