#include "Resolve.hpp"

namespace Ion
{
	Type *type_alloc(Type::Kind kind)
	{
		Type *t{ reinterpret_cast<Type *>(xcalloc(1, sizeof(Type))) };
		t->kind = kind;
		return t;
	}

	Type type_void_val{ Type::VOID, 0, 0 };
	Type type_char_val{ Type::CHAR, 1, 1 };
	Type type_int_val{ Type::INT, 4, 4 };
	Type type_float_val{ Type::FLOAT, 4, 4 };

	Type *type_void{ &type_void_val };
	Type *type_char{ &type_char_val };
	Type *type_int{ &type_int_val };
	Type *type_float{ &type_float_val };
	const size_t PTR_SIZE{ 8 };
	const size_t PTR_ALIGN{ 8 };

	size_t type_sizeof(Type *type)
	{
		assert(type->kind > Type::COMPLETING);
		assert(type->size != 0);
		return type->size;
	}
	size_t type_alignof(Type *type)
	{
		assert(type->kind > Type::COMPLETING);
		assert(IS_POW2(type->align));
		return type->align;
	}
	std::vector<CachedPtrType> cached_ptr_types;
	Type *type_ptr(Type *elem)
	{
		for (CachedPtrType &it : cached_ptr_types)
			if (it.elem == elem)
				return it.ptr;
		Type *t{ type_alloc(Type::PTR) };
		t->size = PTR_SIZE;
		t->align = PTR_ALIGN;
		t->ptr.elem = elem;
		cached_ptr_types.push_back({ elem, t });
		return t;
	}
	std::vector<CachedArrayType> cached_array_types;
	Type *type_array(Type *elem, size_t size)
	{
		for (CachedArrayType &it : cached_array_types)
			if (it.elem == elem && it.size == size)
				return it.array;
		complete_type(elem);
		Type *t = type_alloc(Type::ARRAY);
		t->size = size * type_sizeof(elem);
		t->align = type_alignof(elem);
		t->array.elem = elem;
		t->array.size = size;
		cached_array_types.push_back({ elem, size, t });
		return t;
	}
	std::vector<CachedFuncType> cached_func_types;
	Type *type_func(std::vector<Type*> params, Type *ret)
	{
		for (CachedFuncType &it : cached_func_types)
			if (it.params.size() == params.size() && it.ret == ret && it.params == params)
				return it.func;
		Type *t{ type_alloc(Type::FUNC) };
		t->size = PTR_SIZE;
		t->align = PTR_ALIGN;
		t->func.params = params;
		t->func.ret = ret;
		cached_func_types.push_back({ params, ret, t });
		return t;
	}
	bool duplicate_fields(std::vector<TypeField> fields)
	{
		for (size_t i = 0; i < fields.size(); ++i)
			for (size_t j = i + 1; j < fields.size(); ++j)
				if (fields[i].name == fields[j].name)
					return true;
		return false;
	}
	void type_complete_struct(Type *type, std::vector<TypeField> fields)
	{
		assert(type->kind == Type::COMPLETING);
		type->kind = Type::STRUCT;
		type->size = 0;
		type->align = 0;
		for (TypeField &it : fields)
		{
			type->size = type_sizeof(it.type) + ALIGN_UP(type->size, type_alignof(it.type));
			type->align = std::max(type->align, type_alignof(it.type));
		}
		type->aggregate.fields = fields;
	}
	void type_complete_union(Type *type, std::vector<TypeField> fields)
	{
		assert(type->kind == Type::COMPLETING);
		type->kind = Type::UNION;
		type->size = 0;
		type->align = 0;
		for (TypeField &it : fields)
		{
			assert(it.type->kind > Type::COMPLETING);
			type->size = std::max(type->size, type_sizeof(it.type));
			type->align = std::max(type->align, type_alignof(it.type));
		}
		type->aggregate.fields = fields;
	}
	Type *type_incomplete(Sym *sym)
	{
		Type *type = type_alloc(Type::INCOMPLETE);
		type->sym = sym;
		return type;
	}
	std::vector<Sym*> global_syms;
	Sym* local_syms[MAX_LOCAL_SYMS];
	Sym **local_syms_end = &local_syms;
	Sym *sym_new(Sym::Kind kind, const char *name, Decl *decl)
	{
		Sym *e{ reinterpret_cast<Sym *>(xcalloc(1, sizeof(Decl))) };
		e->kind = kind;
		e->name = name;
		e->decl = decl;
		return e;
	}
	Sym *sym_decl(Decl *decl)
	{
		Sym::Kind kind{ Sym::NONE };
		switch (decl->kind) {
		case Decl::STRUCT: case Decl::UNION: case Decl::TYPEDEF: case Decl::ENUM:
			kind = Sym::TYPE; break;
		case Decl::VAR: kind = Sym::VAR; break;
		case Decl::CONST: kind = Sym::CONST; break;
		case Decl::FUNC: kind = Sym::FUNC; break;
		default: assert(0); break;
		}
		Sym *sym = sym_new(kind, decl->name, decl);
		if (decl->kind == Decl::STRUCT || decl->kind == Decl::UNION) {
			sym->state = Sym::RESOLVED;
			sym->type = type_incomplete(sym);
		}
		return sym;
	}
	Sym *sym_enum_const(const char *name, Decl *decl)
	{
		return sym_new(Sym::ENUM_CONST, name, decl);
	}
	Sym *sym_get(const char *name)
	{
		for (Sym **it = local_syms_end; it != local_syms; --it)
		{
			Sym *sym = it[-1];
			if (sym->name == name)
				return sym;
		}
		for (Sym *it : global_syms)
			if (it->name == name)
				return it;
		return nullptr;
	}
	Sym *sym_install_decl(Decl *decl)
	{
		Sym *e{ sym_decl(decl) };
		global_syms.emplace_back(e);
		if (decl->kind == Decl::ENUM)
			for (EnumItem &it : decl->enum_decl.items)
				global_syms.emplace_back(sym_enum_const(it.name, decl));
		return e;
	}
	Sym *sym_install_type(const char *name, Type *type)
	{
		Sym *e{ sym_new(Sym::TYPE, name, nullptr) };
		e->state = Sym::RESOLVED;
		e->type = type;
		global_syms.push_back(e);
		return e;
	}
	ResolvedExpr resolved_null;
	
	Type * resolve_typespec(Typespec *typespec)
	{
		switch (typespec->kind)
		{
		case Typespec::NAME:
		{
			Sym *sym{ resolve_name(typespec->name) };
			if (sym->kind != Sym::TYPE)
			{
				fatal("%s must denote a type", typespec->name);
				return nullptr;
			}
			return sym->type;
		}
		case Typespec::PTR: return type_ptr(resolve_typespec(typespec->ptr.elem));
		case Typespec::ARRAY:
		{
			int64_t size = resolve_const_expr(typespec->array.size);
			if (size < 0) fatal("Negative array size");
			return type_array(resolve_typespec(typespec->array.elem), size);
		}
		case Typespec::FUNC:
		{
			std::vector<Type *> args;
			for (Typespec *it : typespec->func.args)
				args.emplace_back(resolve_typespec(it));
			Type *ret = type_void;
			if (typespec->func.ret)
				ret = resolve_typespec(typespec->func.ret);
			return type_func(args, ret);
		}
		default: assert(0); return nullptr;
		}
	}
	std::vector<Sym*> ordered_syms;

	void complete_type(Type *type)
	{
		if (type->kind == Type::COMPLETING)
		{
			fatal("Type completion cycle"); return;
		}
		else if (type->kind != Type::INCOMPLETE) { return; }
		type->kind = Type::COMPLETING;
		Decl *decl{ type->sym->decl };
		assert(decl->kind == Decl::STRUCT || decl->kind == Decl::UNION);
		std::vector<TypeField> fields;
		for (AggregateItem &item : decl->aggregate.items)
		{
			Type *item_type{ resolve_typespec(item.type) };
			complete_type(item_type);
			for (const char * name : item.names)
				fields.emplace_back(TypeField{ name, item_type });
		}
		if (fields.size() == 0)
			fatal("No fields");
		if(duplicate_fields(fields))
			fatal("Duplicate fields");
		if (decl->kind == Decl::STRUCT) { type_complete_struct(type, fields); }
		else
		{
			assert(decl->kind == Decl::UNION);
			type_complete_union(type, fields);
		}
		ordered_syms.emplace_back(type->sym);
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
			type = resolve_typespec(decl->var.type);
		if (decl->var.expr)
		{
			ResolvedExpr result = resolve_expr(decl->var.expr, type);
			if (type && result.type != type)
				fatal("Declared var type does not match inferred type");
			type = result.type;
		}
		complete_type(type);
		return type;
	}
	Type *resolve_decl_const(Decl *decl, int64_t *val)
	{
		assert(decl->kind == Decl::CONST);
		ResolvedExpr result = resolve_expr(decl->const_decl.expr);
		if (!result.is_const)
			fatal("Initializer for const is not a constant expression");
		*val = result.val;
		return result.type;
	}
	Type *resolve_decl_func(Decl *decl)
	{
		assert(decl->kind == Decl::FUNC);
		std::vector<Type*> params;
		params.reserve(decl->func.params.size());
		for (FuncParam &it : decl->func.params)
			params.push_back(resolve_typespec(it.type));
		Type *ret_type = type_void;
		if (decl->func.ret_type)
			ret_type = resolve_typespec(decl->func.ret_type);
		return type_func(params, ret_type);
	}
	void resolve_sym(Sym *sym)
	{
		if (sym->state == Sym::RESOLVED) { return; }
		else if (sym->state == Sym::RESOLVING)
		{
			fatal("Cyclic dependency"); return;
		}
		assert(sym->state == Sym::UNRESOLVED);
		sym->state = Sym::RESOLVING;
		switch (sym->kind) {
		case Sym::TYPE: sym->type = resolve_decl_type(sym->decl); break;
		case Sym::VAR: sym->type = resolve_decl_var(sym->decl); break;
		case Sym::CONST: sym->type = resolve_decl_const(sym->decl, &sym->val); break;
		case Sym::FUNC: sym->type = resolve_decl_func(sym->decl); break;
		default: assert(0); break;
		}
		sym->state = Sym::RESOLVED;
		ordered_syms.emplace_back(sym);
	}
	void complete_sym(Sym *sym)
	{
		resolve_sym(sym);
		if (sym->kind == Sym::TYPE)
			complete_type(sym->type);
	}

	Sym *resolve_name(const char *name)
	{
		Sym *sym{ sym_get(name) };
		if (!sym)
		{
			fatal("Non-existent name"); return nullptr;
		}
		resolve_sym(sym);
		return sym;
	}

	ResolvedExpr resolve_expr_field(Expr *expr)
	{
		assert(expr->kind == Expr::FIELD);
		ResolvedExpr left{ resolve_expr(expr->field.expr) };
		Type *type{ left.type };
		complete_type(type);
		if (type->kind != Type::STRUCT && type->kind != Type::UNION)
		{
			fatal("Can only access fields on aggregate types");
			return resolved_null;
		}
		for (TypeField &it : type->aggregate.fields)
		{
			if (it.name == expr->field.name)
				return left.is_lvalue ? resolved_lvalue(it.type) : resolved_rvalue(it.type);
		}
		fatal("No field named '%s'", expr->field.name);
		return resolved_null;
	}
	ResolvedExpr ptr_decay(ResolvedExpr expr)
	{
		if (expr.type->kind == Type::ARRAY)
			return resolved_rvalue(type_ptr(expr.type->array.elem));
		else
			return expr;
	}
	ResolvedExpr resolve_expr_name(Expr *expr)
	{
		assert(expr->kind == Expr::NAME);
		Sym *sym{ resolve_name(expr->name) };
		switch (sym->kind)
		{
		case Sym::VAR: return resolved_lvalue(sym->type);
		case Sym::CONST: return resolved_const(sym->val);
		case Sym::FUNC: return resolved_rvalue(sym->type);
		default: 
			fatal("%s must be a var or const", expr->name);
			return resolved_null;
		}
	}

	int64_t eval_int_unary(Token::Kind op, int64_t val)
	{
		switch (op) 
		{
		case Token::ADD: return +val;
		case Token::SUB: return -val;
		case Token::NEG: return ~val;
		case Token::NOT: return !val;
		default: assert(0); return 0;
		}
	}
	int64_t eval_int_binary(Token::Kind op, int64_t left, int64_t right)
	{
		switch (op)
		{
		case Token::MUL:	return left * right;
		case Token::DIV:	return right == 0 ? 0 : left / right;
		case Token::MOD:	return right == 0 ? 0 : left % right;
		case Token::AND:	return left & right;
			// TODO: Don't allow UB in shifts, etc
		case Token::LSHIFT:	return left << right;
		case Token::RSHIFT:	return left >> right;
		case Token::ADD:	return left + right;
		case Token::SUB:	return left - right;
		case Token::OR:		return left | right;
		case Token::XOR:	return left ^ right;
		case Token::EQ:		return left == right;
		case Token::NOTEQ:	return left != right;
		case Token::LT:		return left < right;
		case Token::LTEQ:	return left <= right;
		case Token::GT:		return left > right;
		case Token::GTEQ:	return left >= right;
			// TODO: Probably handle logical AND/OR separately 
		case Token::AND_AND:return left && right;
		case Token::OR_OR:	return left || right;
		default: assert(0); return 0;
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
			operand = ptr_decay(operand);
			if (type->kind != Type::PTR)
				fatal("Cannot deref non-ptr type");
			return resolved_lvalue(type->ptr.elem);
		case Token::AND:
			if (!operand.is_lvalue)
				fatal("Cannot take address of non-lvalue");
			return resolved_rvalue(type_ptr(type));
		default:
			if (type->kind != Type::INT)
				fatal("Can only use unary %s with int's", token_kind_name(expr->unary.op));
			if (operand.is_const)
				return resolved_const(eval_int_unary(expr->unary.op, operand.val));
			else return resolved_rvalue(type);
		}
	}
	ResolvedExpr resolve_expr_binary(Expr *expr)
	{
		assert(expr->kind == Expr::BINARY);
		ResolvedExpr left{ resolve_expr(expr->binary.left) };
		ResolvedExpr right{ resolve_expr(expr->binary.right) };
		if (left.type != type_int)
			fatal("left operand of + must be int");
		if (right.type != left.type)
			fatal("left and right operand of + must have same type");
		if (left.is_const && right.is_const)
			return resolved_const(eval_int_binary(expr->binary.op, left.val, right.val));
		else
			return resolved_rvalue(left.type);
	}
	size_t aggregate_field_index(Type *type, const char *name)
	{
		assert(type->kind == Type::STRUCT || type->kind == Type::UNION);
		for (size_t i = 0; i < type->aggregate.fields.size(); ++i)
			if (type->aggregate.fields[i].name == name)
				return i;

		fatal("Field '%s' in compound literal not found in struct/union", name);
		return SIZE_MAX;
	}
	ResolvedExpr resolve_expr_compound(Expr *expr, Type *expected_type)
	{
		assert(expr->kind == Expr::COMPOUND);
		if (!expected_type && !expr->compound.type)
			fatal("Implicitly typed compound literals used in context without expected types");
		Type *type{ nullptr };
		if (expr->compound.type)
			type = resolve_typespec(expr->compound.type);
		else
			type = expected_type;
		complete_type(type);
		if (type->kind != Type::STRUCT && type->kind != Type::UNION && type->kind != Type::ARRAY)
			fatal("Compound literals can only be used with struct, union and array types");

		if (type->kind == Type::STRUCT || type->kind == Type::UNION)
		{
			size_t index{ 0 };
			for (size_t i{ 0 }; i < expr->compound.fields.size(); ++i)
			{
				CompoundField field{ expr->compound.fields[i] };
				if (field.kind == CompoundField::INDEX)
					fatal("Index field initializer not allowed for struct/union compound literal");
				else if (field.kind == CompoundField::NAME)
					index = aggregate_field_index(type, field.name);

				if (index >= type->aggregate.fields.size())
					fatal("Field initializer in struct/union compound literal out of range");
				ResolvedExpr init = resolve_expr(expr->compound.fields[i].init, type->aggregate.fields[index].type);
				if (init.type != type->aggregate.fields[index].type)
					fatal("Compound literal field type mismatch");
				++index;
			}
		}
		else
		{
			assert(type->kind == Type::ARRAY);
			size_t index{ 0 };
			for (size_t i{ 0 }; i < expr->compound.fields.size(); ++i)
			{
				CompoundField field{ expr->compound.fields[i] };
				if (field.kind == CompoundField::NAME)
					fatal("Named field initializer not allowed for array compound literals");
				else if (field.kind == CompoundField::INDEX)
				{
					int64_t result = resolve_const_expr(field.index);
					if(result < 0)
						fatal("Field initializer index cannot be negative");
					index = result;
				}
				if (index >= type->array.size)
					fatal("Field initializer in array compound literal out of range");
				ResolvedExpr init = resolve_expr(expr->compound.fields[i].init, type->array.elem);
				if (init.type != type->array.elem)
					fatal("Compound literal element type mismatch");
				++index;
			}
		}
		return resolved_rvalue(type);
	}
	ResolvedExpr resolve_expr_call(Expr *expr)
	{
		assert(expr->kind == Expr::CALL);
		ResolvedExpr func{ resolve_expr(expr->call.expr) };
		if (func.type->kind != Type::FUNC)
			fatal("Trying to call non-function value");
		if (expr->call.args.size() != func.type->func.params.size())
			fatal("Tried to call function with wrong number of arguments");
		for (size_t i{ 0 }; i < expr->call.args.size(); ++i)
		{
			Type *param_type{ func.type->func.params[i] };
			ResolvedExpr arg{ resolve_expr(expr->call.args[i], param_type) };
			if (arg.type != param_type)
				fatal("Call argument expression type doesn't match expected param type");
		}

		return resolved_rvalue(func.type->func.ret);
	}
	ResolvedExpr resolve_expr_ternary(Expr *expr, Type *expected_type)
	{
		assert(expr->kind == Expr::TERNARY);
		ResolvedExpr cond{ ptr_decay(resolve_expr(expr->ternary.cond)) };
		
		if (cond.type->kind != Type::INT && cond.type->kind != Type::PTR)
			fatal("Ternary cond expression must have type int or ptr");
		ResolvedExpr then_expr = ptr_decay(resolve_expr(expr->ternary.then_expr, expected_type));
		ResolvedExpr else_expr = ptr_decay(resolve_expr(expr->ternary.else_expr, expected_type));
		if (then_expr.type != else_expr.type)
			fatal("Ternary then/else expr must have matching types");
		if (cond.is_const && then_expr.is_const && else_expr.is_const)
			return resolved_const(cond.val ? then_expr.val : else_expr.val);
		else return resolved_rvalue(then_expr.type);
	}
	ResolvedExpr resolve_expr_index(Expr *expr)
	{
		assert(expr->kind == Expr::INDEX);
		ResolvedExpr operand{ ptr_decay(resolve_expr(expr->index.expr)) };
		if (operand.type->kind != Type::PTR)
			fatal("Can only index arrays or pointers");
		ResolvedExpr index{ resolve_expr(expr->index.index) };
		if (index.type->kind != Type::INT)
			fatal("Index expressions must have type int");
		return resolved_lvalue(operand.type->ptr.elem);
	}
	ResolvedExpr resolve_expr_cast(Expr *expr)
	{
		assert(expr->kind == Expr::CAST);
		Type *type{ resolve_typespec(expr->cast.type) };
		ResolvedExpr result{ ptr_decay(resolve_expr(expr->cast.expr)) };
		if (type->kind == Type::PTR)
		{
			if (result.type->kind != Type::PTR && result.type->kind != Type::INT)
				fatal("invalid cast to pointer type");
		}
		else if (type->kind = Type::INT)
		{
			if (result.type->kind != Type::PTR && result.type->kind != Type::INT)
				fatal("Invalid cast to int type");
		}
		else fatal("Invalid target cast type");
		
		return resolved_rvalue(type);
	}
	ResolvedExpr resolve_expr(Expr *expr, Type *expected_type)
	{
		switch (expr->kind) {
		case Expr::INT: return resolved_const(expr->int_val);
		case Expr::FLOAT: return resolved_rvalue(type_float);
		case Expr::STR: return resolved_rvalue(type_ptr(type_char));
		case Expr::NAME: return resolve_expr_name(expr);
		case Expr::CAST: return resolve_expr_cast(expr);
		case Expr::CALL: return resolve_expr_call(expr);
		case Expr::INDEX: return resolve_expr_index(expr);
		case Expr::FIELD: return resolve_expr_field(expr);
		case Expr::COMPOUND: return resolve_expr_compound(expr, expected_type);
		case Expr::UNARY: return resolve_expr_unary(expr);
		case Expr::BINARY: return resolve_expr_binary(expr);
		case Expr::TERNARY: return resolve_expr_ternary(expr, expected_type);
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
	int64_t resolve_const_expr(Expr *expr)
	{
		ResolvedExpr result{ resolve_expr(expr) };
		if (!result.is_const)
			fatal("Expected constant expression");
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

		sym_install_type(str_intern("int"), type_int);
		sym_install_type(str_intern("char"), type_char);
		sym_install_type(str_intern("void"), type_void);

		const char *code[] = {
			"union IntOrPtr { i: int; p: int*; }",
			"var u1 = IntOrPtr{i = 42}",
			"var u2 = IntOrPtr{p = cast(int*, 42)}",
			"var a: int[256] = {1, 2, ['a'] = 42, [255] = 123}",
			/*
			"struct Vector { x, y: int; }",
			"func add(v: Vector, w: Vector): Vector { return {v.x + w.x, v.y + w.y}; }",
			"var v: Vector = 0 ? {1,2} : {3,4}",
			"var vs: Vector[2][2] = {{{1,2},{3,4}}, {{5,6},{7,8}}}",
			"struct A { c: char; }",
			"struct B { i: int; }",
			"struct C { c: char; a: A; }",
			"struct D { c: char; b: B; }",
			"func print(v: Vector) { printf(\"{%d, %d}\", v.x, v.y); }",
			"var x = add({1,2}, {3,4})",
			"var v: Vector = {1,2}",
			"var w = Vector{3,4}",
			"var p: void*",
			"var i = cast(int, p) + 1",
			"var fp: func(Vector)",
			"struct Dup { x: int; x: int; }",
			"var a: int[3] = {1,2,3}",
			"var b: int[4]",
			"var p = &a[1]",
			"var i = p[1]",
			"var j = *p",
			"const n = sizeof(a)",
			"const m = sizeof(&a[0])",
			"const l = sizeof(1 ? a : b)",
			"var pi = 3.14",
			"var name = \"Per\"",
			"var v = Vector{1,2}",
			"var j = cast(int, p)",
			"var q = cast(int*, j)",
			"const i = 42",
			"const j = +i",
			"const k = -i",
			"const a = 1000/((2*3-5) << 1)",
			"const b = !0",
			"const c = ~100 + 1 == -100",
			"const k = 1 ? 2 : 3",
			"union IntOrPtr { i: int; p: int*; }",
			"var i = 42",
			"var u = IntOrPtr{i, &i}",
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
			"const n = sizeof(x)",
			"var x: T",
			"struct T { s: S*; }",
			"struct S { t: T[n]; }",*/
			
		};
		for (size_t i = 0; i < sizeof(code) / sizeof(*code); ++i)
		{
			init_stream(code[i]);
			Decl *decl = parse_decl();
			sym_install_decl(decl);
		}
		for (Sym *it : global_syms)
		{
			complete_sym(it);
		}
		for (Sym *it : ordered_syms)
		{
			if (it->decl) print_decl(it->decl);
			else PRINTF("%s", it->name);
			PRINTF("\n");
		}
 	}
}
