#include <stdio.h>
#include <string.h>
#pragma warning(disable : 4996)

extern "C" int parser_main(int argc, char* argv[]);

int main(int argc, char* argv[])
{
	return parser_main(argc, argv);
}