/*H*
 * 
 * FILENAME: interpreter.c
 * DESCRIPTION: Interpreter
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 25.01.2019
 * COMPILING: gcc interpreter.c -I/usr/include -L interpreter.h structures.c structures.h tokenizer.c tokenizer.h -o logic
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
	return 0;
}