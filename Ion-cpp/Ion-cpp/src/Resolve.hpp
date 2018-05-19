#pragma once
#include "stdafx.h"
#include "Parse.hpp"

namespace Ion
{
	struct Sym
	{
		const char *name;
		Decl *decl;

		enum
		{
			UNRESOLVED,
			RESOLVING,
			RESOLVED
		} state;
	};

	std::vector<Sym> sym_list;

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

	// https://youtu.be/0WpCnd9E-eg?t=2401

	static void resolve_test()
	{
		const char *foo{ str_intern("foo") };
		assert(sym_get(foo) == nullptr);
		Decl *decl{ decl_const(foo, expr_int(42)) };
		sym_put(decl);
		Sym *sym{ sym_get(foo) };
		assert(sym && sym->decl == decl);
	}
}