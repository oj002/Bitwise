#pragma once
#define ALIGN_DOWN(n, a) ((n) & ~((a) - 1))
#define ALIGN_UP(n, a) ALIGN_DOWN((n) + (a) - 1, (a))
#define ALIGN_DOWN_PTR(p, a) ((void *)ALIGN_DOWN((uintptr_t)(p), (a)))
#define ALIGN_UP_PTR(p, a) ((void *)ALIGN_UP((uintptr_t)(p), (a)))

#define IS_POW2(x) (((x) != 0) && ((x) & ((x)-1)) == 0)

void *xcalloc(size_t num_elems, size_t elem_size)
{
	void *ret{ calloc(num_elems, elem_size) };
	if (!ret)
	{
		perror("xcalloc failed");
		exit(1);
	}
	return ret;
}
void *xrealloc(void *ptr, size_t num_bytes)
{
	void *ret{ realloc(ptr, num_bytes) };
	if (!ret)
	{
		perror("xrealloc failed");
		exit(1);
	}
	return ret;
}
void *xmalloc(size_t num_bytes)
{
	void *ret{ malloc(num_bytes) };
	if (!ret)
	{
		perror("xmalloc failed");
		exit(1);
	}
	return ret;
}

void fatal(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	printf("FATAL: ");
	printf(fmt, args);
	printf("\n");
	va_end(args);
	assert(0);
	exit(1);
}
void syntax_error(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	printf("Syntax Error: ");
	printf(fmt, args);
	printf("\n");
	va_end(args);
}
void fatal_syntax_error(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	printf("Syntax Error: ");
	printf(fmt, args);
	printf("\n");
	va_end(args);
	assert(0);
	exit(1);
}
char *strf(const char *fmt, ...)
{
	va_list args;
	
	va_start(args, fmt);
	size_t n = 1 + vsnprintf(NULL, 0, fmt, args);
	va_end(args);
	
	char *str = (char*)xmalloc(n);
	
	va_start(args, fmt);
	vsnprintf(str, n, fmt, args);
	va_end(args);

	return str;
}

struct Arena
{
	char *ptr;
	char *end;
	std::vector<char *> blocks;
};

#define ARENA_ALIGNMENT 8
#define ARENA_BLOCK_SIZE 1024
void arena_grow(Arena *arena, size_t min_size)
{
	const size_t size{ ALIGN_UP(std::max<size_t>(ARENA_BLOCK_SIZE, min_size), ARENA_ALIGNMENT) };
	arena->ptr = static_cast<char *>(xmalloc(size));
	arena->end = arena->ptr + size;
	arena->blocks.push_back(arena->ptr);

}
void *arena_alloc(Arena *arena, size_t size)
{
	if (size > static_cast<size_t>(arena->end - arena->ptr))
	{
		arena_grow(arena, size);
		assert(size <= static_cast<size_t>(arena->end - arena->ptr));
	}
	void *ptr{ arena->ptr };
	arena->ptr = static_cast<char *>(ALIGN_UP_PTR(arena->ptr + size, ARENA_ALIGNMENT));
	assert(arena->ptr <= arena->end);
	assert(ptr == ALIGN_DOWN_PTR(ptr, ARENA_ALIGNMENT));
	return ptr;
}
void arena_free(Arena *arena)
{
	for (char *c : arena->blocks)
		free(c);
}

struct InternStr
{
	InternStr(size_t len, const char *str)
		: len(len), str(str)
	{}

	size_t len;
	const char *str;
};

Arena str_arena;
std::vector<InternStr> interns;

const char *str_intern_range(const char *start, const char *end)
{
	uint64_t len{ static_cast<uint64_t>(end - start) };
	for (const InternStr &inStr : interns)
	{
		if (inStr.len == len && strncmp(inStr.str, start, len) == 0)
		{
			return inStr.str;
		}
	}
	auto *str = reinterpret_cast<char*>(arena_alloc(&str_arena, len + 1));
	memcpy(str, start, len);
	str[len] = 0;
	interns.emplace_back(len, const_cast<const char*>(str));
	return str;
}
const char *str_intern(const char *str)
{
	return str_intern_range(str, str + strlen(str));
}

void str_intern_test()
{
	char a[] = "hello";
	assert(strcmp(a, str_intern(a)) == 0);
	assert(str_intern(a) == str_intern(a));
	assert(str_intern(str_intern(a)) == str_intern(a));
	char b[] = "hello";
	assert(a != b);
	assert(str_intern(a) == str_intern(b));
	char c[] = "hello!";
	assert(str_intern(a) != str_intern(c));
	char d[] = "hell";
	assert(str_intern(a) != str_intern(d));
}

void common_test()
{
	str_intern_test();

	char *str1 = strf("%d %d", 1, 2);
	assert(strcmp(str1, "1 2") == 0);
	char *str2 = strf("%s %s", str1, str1);
	assert(strcmp(str2, "1 2 1 2") == 0);
	char *str3 = strf("%s asdf %s", str2, str2);
	assert(strcmp(str3, "1 2 1 2 asdf 1 2 1 2") == 0);
}

