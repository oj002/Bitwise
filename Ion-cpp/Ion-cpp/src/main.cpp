#include "Resolve.hpp"
#include <stdbool.h>

// https://youtu.be/ls_YmJ21JZg?list=PLU94OURih-ChlhR_dQ_nfcZoyteSPYfNx&t=5166

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
