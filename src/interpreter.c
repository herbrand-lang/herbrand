/*H*
 * 
 * FILENAME: interpreter.c
 * DESCRIPTION: Interpreter
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 27.01.2019
 * COMPILING: gcc -I/usr/include -L *.h *.c -o logic -g
 * 
 *H*/

 #include "interpreter.h"



/**
  * 
  * This function ...
  * 
  **/
int main(int argc, char *argv[]) {
	Program *program;
	FILE *file;
	file = fopen("../sample/append.lo", "r");
	program = parser_stream(file);
	program_listing(program);
	program_print(program);
	return 0;
}
