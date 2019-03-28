/*H*
 * 
 * FILENAME: main.c
 * DESCRIPTION: Main file
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 27.03.2019
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
	Program *program;
	FILE *file;
	if(argc == 2) {
		file = fopen(argv[1], "r");
		if(file != NULL) {
			program = program_alloc();
			parser_stream(program, file);
			printf("\nPARSED RULES:\n");
			program_print(program);
			printf("\nHASHMAP RULES:\n");
			hashmap_print(program->indices);
		} else {
			printf("(existence-error file \"%s\")\n", argv[1]);
		}
		program_free(program);
	} else {
		interactive_mode();
	}
	return 0;
}

/**
  * 
  * This function starts the interactive mode.
  * 
  */
void interactive_mode() {
	Term *term1 = NULL, *term2 = NULL;
	Substitution *mgu;
	while(1) {
		while(term1 == NULL)
			term1 = parser_term(stdin);
		while(term2 == NULL)
			term2 = parser_term(stdin);
		mgu = semantics_unify_terms(term1, term2, 0);
		if(mgu != NULL) {
			substitution_print(mgu);
			printf("\n");
			substitution_free(mgu);
		} else {
			printf("false\n");
		}
		term_free(term1);
		term_free(term2);
		term1 = NULL;
		term2 = NULL;
	}
}