#include "Resolve.hpp"

// TODO: no more assert use exceptions
// https://youtu.be/0WpCnd9E-eg?t=182
// https://github.com/pervognsen/bitwise/tree/1f5bf73238c81f4b0d290d013b459932e5fa7122/ion
int main()
{
	Ion::common_test();
	Ion::lex_test();
	// Ion::print_test();
	Ion::parse_test();
	Ion::resolve_test();
	system("pause");
	return 0;
}
