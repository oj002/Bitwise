#pragma once
#include "stdafx.h"
#define ALIGN_DOWN(n, a) ((n) & ~((a) - 1))
#define ALIGN_UP(n, a) ALIGN_DOWN((n) + (a) - 1, (a))
#define ALIGN_DOWN_PTR(p, a) ((void *)ALIGN_DOWN((uintptr_t)(p), (a)))
#define ALIGN_UP_PTR(p, a) ((void *)ALIGN_UP((uintptr_t)(p), (a)))

#define IS_POW2(x) (((x) != 0) && ((x) & ((x)-1)) == 0)

namespace Ion
{
	void *xcalloc(size_t num_elems, size_t elem_size);
	void *xrealloc(void *ptr, size_t num_bytes);
	void *xmalloc(size_t num_bytes);

	void fatal(const char *fmt, ...);
	void syntax_error(const char *fmt, ...);
	void fatal_syntax_error(const char *fmt, ...);

	struct Arena
	{
		char *ptr;
		char *end;
		std::vector<char *> blocks;
	};

	#define ARENA_ALIGNMENT 8
	#define ARENA_BLOCK_SIZE 1024
	void arena_grow(Arena *arena, size_t min_size);
	void *arena_alloc(Arena *arena, size_t size);
	void arena_free(Arena *arena);

	struct InternStr
	{
		InternStr(size_t len, const char *str)
			: len(len), str(str)
		{}

		size_t len;
		const char *str;
	};

	static Arena str_arena;
	static std::vector<InternStr> interns;

	const char *str_intern_range(const char *start, const char *end);
	const char *str_intern(const char *str);

	void str_intern_test();
	void common_test();
} // namespace Ion