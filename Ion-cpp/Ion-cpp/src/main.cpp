#include "Resolve.hpp"
#include <stdbool.h>

// https://youtu.be/VRMxHYuW2BY

int main()
{
	Ion::common_test();
	Ion::lex_test();
	// Ion::print_test();
	// Ion::parse_test();
	Ion::resolve_test();

	system("pause");
	return 0;
}
