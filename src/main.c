/*H*
 * 
 * FILENAME: main.c
 * DESCRIPTION: Main file
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 27.01.2019
 * COMPILING: gcc -I/usr/include -L *.h *.c -o logic -g
 * 
 *H*/

#include "main.h"



/**
  * 
  * This function initilizes the interpreter.
  * 
  **/
int main(int argc, char *argv[]) {
	char character;
	Program *program = program_alloc();
	FILE *file;
	if(argc == 2) {
		file = fopen(argv[1], "r");
		if(file != NULL) {
			parser_stream(program, file);
			printf("\nPARSED RULES:\n");
			program_print(program);
			printf("\nHASHMAP RULES:\n");
			hashmap_print(program->indices);
		} else {
			printf("(existence-error file \"%s\")\n", argv[1]);
		}
	}
	program_free(program);
	return 0;
}