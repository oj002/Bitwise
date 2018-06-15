#include "Resolve.hpp"

namespace Ion
{
	Type *type_alloc(Type::Kind kind)
	{
		Type *t{ reinterpret_cast<Type *>(xcalloc(1, sizeof(Type))) };
		t->kind = kind;
		return t;
	}

	Type type_int_val{ Type::INT, 4 };
	Type type_float_val{ Type::FLOAT, 4 };
	Type *type_int{ &type_int_val };
	Type *type_float{ &type_float_val };
	const size_t PTR_SIZE{ 8 };

	size_t type_sizeof(Type *type)
	{
		assert(type->kind > Type::COMPLETING);
		assert(type->size != 0);
		return type->size;
	}
	std::vector<CachedPtrType> cached_ptr_types;
	Type *type_ptr(Type *elem)
	{
		for (CachedPtrType &it : cached_ptr_types)
		{
			if (it.elem == elem)
			{
				return it.ptr;
			}
		}
		Type *t{ type_alloc(Type::PTR) };
		t->size = PTR_SIZE;
		t->ptr.elem = elem;
		cached_ptr_types.push_back({ elem, t });
		return t;
	}
	std::vector<CachedArrayType> cached_array_types;
	Type *type_array(Type *elem, size_t size)
	{
		for (CachedArrayType &it : cached_array_types)
		{
			if (it.elem == elem && it.size == size)
			{
				return it.array;
			}
		}
		complete_type(elem);
		Type *t = type_alloc(Type::ARRAY);
		t->size = size * type_sizeof(elem);
		t->array.elem = elem;
		t->array.size = size;
		cached_array_types.push_back({ elem, size, t });
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
			if (it.params.size() == params.size() && it.ret == ret && it.params == params)
			{
				return it.func;
			}
		}
		Type *t{ type_alloc(Type::FUNC) };
		t->size = PTR_SIZE;
		t->func.params = params;
		t->func.ret = ret;
		cached_func_types.push_back({ params, ret, t });
		return t;
	}
	void type_complete_struct(Type *type, std::vector<TypeField> fields)
	{
		assert(type->kind == Type::COMPLETING);
		type->kind = Type::STRUCT;
		type->size = 0;
		for (TypeField &it : fields)
		{
			type->size += type_sizeof(it.type);
		}
		type->aggregate.fields = fields;
	}
	void type_complete_union(Type *type, std::vector<TypeField> fields)
	{
		assert(type->kind == Type::COMPLETING);
		type->kind = Type::UNION;
		type->size = 0;
		for (TypeField &it : fields)
		{
			assert(it.type->kind > Type::COMPLETING);
			type->size = std::max(type->size, type_sizeof(it.type));
		}
		type->aggregate.fields = fields;
	}
	Type *type_incomplete(Entity *entity)
	{
		Type *type = type_alloc(Type::INCOMPLETE);
		type->entity = entity;
		return type;
	}
	std::vector<Entity*> entities;
	Entity *entity_new(Entity::Kind kind, const char *name, Decl *decl)
	{
		Entity *e{ reinterpret_cast<Entity *>(xcalloc(1, sizeof(Decl))) };
		e->kind = kind;
		e->name = name;
		e->decl = decl;
		return e;
	}
	Entity *entity_decl(Decl *decl)
	{
		Entity::Kind kind{ Entity::NONE };
		switch (decl->kind) {
		case Decl::STRUCT: case Decl::UNION: case Decl::TYPEDEF: case Decl::ENUM:
			kind = Entity::TYPE; break;
		case Decl::VAR: kind = Entity::VAR; break;
		case Decl::CONST: kind = Entity::CONST; break;
		case Decl::FUNC: kind = Entity::FUNC; break;
		default: assert(0); break;
		}
		Entity *entity = entity_new(kind, decl->name, decl);
		if (decl->kind == Decl::STRUCT || decl->kind == Decl::UNION) {
			entity->state = Entity::RESOLVED;
			entity->type = type_incomplete(entity);
		}
		return entity;
	}
	Entity *entity_enum_const(const char *name, Decl *decl)
	{
		return entity_new(Entity::ENUM_CONST, name, decl);
	}
	Entity *entity_get(const char *name)
	{
		for (Entity *it : entities)
		{
			if (it->name == name)
			{
				return it;
			}
		}
		return nullptr;
	}
	Entity *entity_install_decl(Decl *decl)
	{
		Entity *e{ entity_decl(decl) };
		entities.emplace_back(e);
		if (decl->kind == Decl::ENUM)
		{
			for (EnumItem &it : decl->enum_decl.items)
			{
				entities.emplace_back(entity_enum_const(it.name, decl));
			}
		}
		return e;
	}
	Entity *entity_install_type(const char *name, Type *type)
	{
		Entity *e{ entity_new(Entity::TYPE, name, nullptr) };
		e->state = Entity::RESOLVED;
		e->type = type;
		entities.push_back(e);
		return e;
	}
	ResolvedExpr resolved_null;
	
	Type * resolve_typespec(Typespec *typespec)
	{
		switch (typespec->kind)
		{
		case Typespec::NAME:
		{
			Entity *entity{ resolve_name(typespec->name) };
			if (entity->kind != Entity::TYPE)
			{
				fatal("%s must denote a type", typespec->name);
				return nullptr;
			}
			return entity->type;
		}
		case Typespec::PTR: return type_ptr(resolve_typespec(typespec->ptr.elem));
		case Typespec::ARRAY: return type_array(resolve_typespec(typespec->array.elem), resolve_int_const_expr(typespec->array.size));
		case Typespec::FUNC:
		{
			std::vector<Type *> args;
			for (Typespec *it : typespec->func.args)
			{
				args.emplace_back(resolve_typespec(it));
			}
			return type_func(args, resolve_typespec(typespec->func.ret));
		}
		default: assert(0); return nullptr;
		}
	}
	std::vector<Entity*> ordered_entities;

	void complete_type(Type *type)
	{
		if (type->kind == Type::COMPLETING)
		{
			fatal("Type completion cycle"); return;
		}
		else if (type->kind != Type::INCOMPLETE) { return; }
		type->kind = Type::COMPLETING;
		Decl *decl{ type->entity->decl };
		assert(decl->kind == Decl::STRUCT || decl->kind == Decl::UNION);
		std::vector<TypeField> fields;
		for (AggregateItem &item : decl->aggregate.items)
		{
			Type *item_type{ resolve_typespec(item.type) };
			complete_type(item_type);
			for (const char * name : item.names)
			{
				fields.emplace_back(TypeField{ name, item_type });
			}
		}
		if (decl->kind == Decl::STRUCT) { type_complete_struct(type, fields); }
		else
		{
			assert(decl->kind == Decl::UNION);
			type_complete_union(type, fields);
		}
		ordered_entities.emplace_back(type->entity);
	}

	Type *resolve_decl_type(Decl *decl)
	{
		assert(decl->kind == Decl::TYPEDEF);
		return resolve_typespec(decl->typedef_decl.type);
	}
	Type *resolve_decl_var(Decl *decl)
	{
		assert(decl->kind == Decl::VAR);
		Type *type{ nullptr };
		if (decl->var.type)
		{
			type = resolve_typespec(decl->var.type);
		}
		if (decl->var.expr)
		{
			ResolvedExpr result = resolve_expr(decl->var.expr);
			if (type && result.type != type)
			{
				fatal("Declared var type does not match inferred type");
			}
			type = result.type;
		}
		complete_type(type);
		return type;
	}
	Type *resolve_decl_const(Decl *decl, int64_t *val)
	{
		assert(decl->kind == Decl::CONST);
		ResolvedExpr result = resolve_expr(decl->const_decl.expr);
		*val = result.val;
		return result.type;
	}
	void resolve_entity(Entity *entity)
	{
		if (entity->state == Entity::RESOLVED) { return; }
		else if (entity->state == Entity::RESOLVING)
		{
			fatal("Cyclic dependency"); return;
		}
		assert(entity->state == Entity::UNRESOLVED);
		entity->state = Entity::RESOLVING;
		switch (entity->kind) {
		case Entity::TYPE: entity->type = resolve_decl_type(entity->decl); break;
		case Entity::VAR: entity->type = resolve_decl_var(entity->decl); break;
		case Entity::CONST: entity->type = resolve_decl_const(entity->decl, &entity->val); break;
		default: assert(0); break;
		}
		entity->state = Entity::RESOLVED;
		ordered_entities.emplace_back(entity);
	}
	void complete_entity(Entity *entity)
	{
		resolve_entity(entity);
		if (entity->kind == Entity::TYPE)
		{
			complete_type(entity->type);
		}
	}

	Entity *resolve_name(const char *name)
	{
		Entity *entity{ entity_get(name) };
		if (!entity)
		{
			fatal("Non-existent name"); return nullptr;
		}
		resolve_entity(entity);
		return entity;
	}

	ResolvedExpr resolve_expr_field(Expr *expr)
	{
		assert(expr->kind == Expr::FIELD);
		ResolvedExpr left{ resolve_expr(expr->field.expr) };
		Type *type{ left.type };
		complete_type(type);
		if (type->kind != Type::STRUCT && type->kind != Type::UNION)
		{
			fatal("Can only access fields on aggregate types"); return resolved_null;
		}
		for (TypeField &it : type->aggregate.fields)
		{
			if (it.name == expr->field.name)
			{
				return left.is_lvalue ? resolved_lvalue(it.type) : resolved_rvalue(it.type);
			}
		}
		fatal("No field named '%s'", expr->field.name);
		return resolved_null;
	}

	ResolvedExpr resolve_expr_name(Expr *expr)
	{
		assert(expr->kind == Expr::NAME);
		Entity *entity{ resolve_name(expr->name) };
		if (entity->kind == Entity::VAR) { return resolved_lvalue(entity->type); }
		else if (entity->kind == Entity::CONST) { return resolved_const(entity->val); }
		else 
		{
			fatal("%s must be a var or const", expr->name);
			return resolved_null;
		}
	}

	ResolvedExpr resolve_expr_unary(Expr *expr)
	{
		assert(expr->kind == Expr::UNARY);
		ResolvedExpr operand{ resolve_expr(expr->unary.expr) };
		Type *type{ operand.type };
		switch (expr->unary.op) 
		{
		case Token::MUL:
			if (type->kind != Type::PTR) {
				fatal("Cannot deref non-ptr type");
			}
			return resolved_lvalue(type->ptr.elem);
		case Token::AND:
			if (!operand.is_lvalue) {
				fatal("Cannot take address of non-lvalue");
			}
			return resolved_rvalue(type_ptr(type));
		default:
			assert(0);
			return resolved_null;
		}
	}

	ResolvedExpr resolve_expr_binary(Expr *expr)
	{
		assert(expr->kind == Expr::BINARY);
		assert(expr->binary.op == Token::ADD);
		ResolvedExpr left = resolve_expr(expr->binary.left);
		ResolvedExpr right = resolve_expr(expr->binary.right);
		if (left.type != type_int) {
			fatal("left operand of + must be int");
		}
		if (right.type != left.type) {
			fatal("left and right operand of + must have same type");
		}
		if (left.is_const && right.is_const) {
			return resolved_const(left.val + right.val);
		}
		else {
			return resolved_rvalue(left.type);
		}
	}

	ResolvedExpr resolve_expr(Expr *expr)
	{
		switch (expr->kind) {
		case Expr::INT: return resolved_const(expr->int_val);
		case Expr::NAME: return resolve_expr_name(expr);
		case Expr::FIELD: return resolve_expr_field(expr);
		case Expr::UNARY: return resolve_expr_unary(expr);
		case Expr::BINARY: return resolve_expr_binary(expr);
		case Expr::SIZEOF_EXPR:
		{
			ResolvedExpr result = resolve_expr(expr->sizeof_expr);
			Type *type = result.type;
			complete_type(type);
			return resolved_const(type_sizeof(type));
		}
		case Expr::SIZEOF_TYPE:
		{
			Type *type = resolve_typespec(expr->sizeof_type);
			complete_type(type);
			return resolved_const(type_sizeof(type));
		}
		default: assert(0); return resolved_null;
		}
	}

	int64_t resolve_int_const_expr(Expr *expr)
	{
		ResolvedExpr result{ resolve_expr(expr) };
		if (!result.is_const) {
			fatal("Expected constant expression");
		}
		return result.val;
	}

	void resolve_test()
	{
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
		Type *int_int_func = type_func(std::vector<Type*>(1, type_int), type_int);
		assert(type_func(std::vector<Type*>(1, type_int), type_int) == int_int_func);
		Type *int_func = type_func(std::vector<Type*>(0), type_int);
		assert(int_int_func != int_func);
		assert(int_func == type_func(std::vector<Type*>(0), type_int));

		const char *int_name = str_intern("int");
		entity_install_type(int_name, type_int);
		const char *code[] = {
			"const n = 1+sizeof(p)",
			"var p: T*",
			"var u = *p",
			"struct T { a: int[n]; }",
			"var r = &t.a",
			"var t: T",
			"typedef S = int[n+m]",
			"const m = sizeof(t.a)",
			"var i = n+m",
			"var q = &i",
			//        "const n = sizeof(x)",
			//        "var x: T",
			//        "struct T { s: S*; }",
			//        "struct S { t: T[n]; }",
		};
		for (size_t i = 0; i < sizeof(code) / sizeof(*code); ++i)
		{
			init_stream(code[i]);
			Decl *decl = parse_decl();
			entity_install_decl(decl);
		}
		for (Entity *it : entities)
		{
			complete_entity(it);
		}
		for (Entity *it : ordered_entities)
		{
			if (it->decl) { print_decl(it->decl); }
			else
			{ printf("%s", it->name); }
			printf("\n");
		}
	}
}
