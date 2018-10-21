#include "Resolve.hpp"
#include <stdbool.h>

// https://youtu.be/VRMxHYuW2BY?t=2438

int main()
{
	Ion::common_test();
	Ion::lex_test();
	// Ion::print_test();
	// Ion::parse_test();
	Ion::resolve_test();
	//fclose(Ion::flush_print_buff(fopen("test.txt", "w")));

	system("pause");
	return 0;
}
