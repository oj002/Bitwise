#include "stdafx.h"
#include "Common.hpp"
#include "Lex.hpp"
#include "Ast.hpp"
#include "Printing.hpp"
#include "Parse.hpp"
#include "Resolve.hpp"
#include "Gen.hpp"


// https://youtu.be/X9YWYlp8iQg?t=4889
int main()
{
	printf("test\n");
	common_test();
	init_keywords();
	//lex_test();
	//print_test();
	//parse_test();
	//resolve_test();
	//fclose(Ion::flush_print_buff(fopen("test.txt", "w")));
	gen_test();
	system("pause");
	return 0;
}
