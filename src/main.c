/*H*
 * 
 * FILENAME: main.c
 * DESCRIPTION: Main file
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 30.03.2019
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
			fclose(file);
			printf("\nPARSED RULES:\n");
			program_print(program);
			printf("\nHASHMAP RULES:\n");
			hashmap_print(program->indices);
		} else {
			printf("(existence-error file \"%s\")\n", argv[1]);
		}
		program_free(program);
	} else {
		interactive_query();
	}
	return 0;
}

/**
  * 
  * This function starts the interactive
  * query mode.
  * 
  */
void interactive_query() {
	Program *program;
	FILE *file;
	Derivation *D;
	Substitution *answer;
	char c, d;
	Term *term = NULL;
	program = program_alloc();
	file = fopen("../sample/append.lo", "r");
	if(file != NULL) {
		parser_stream(program, file);
		fclose(file);
	}
	while(1) {
		while(term == NULL) {
			printf("logic> ");
			term = parser_term(stdin);
		}
		if(term->type == TYPE_ATOM && strcmp(term->term.string, "exit") == 0) {
			term_free(term);
			break;
		}
		D = semantics_query(term);
		do {
			answer = semantics_answer(program, D);
			substitution_print(answer);
			printf(" ");
			c = getchar();
			while((d = getchar()) != '\n' && d != EOF);
		} while(answer != NULL && c == ';');
		if(answer == NULL)
			printf("\n");
		term_free(term);
		derivation_free(D);
		term = NULL;
	}
	program_free(program);
}

/**
  * 
  * This function starts the interactive
  * unification mode.
  * 
  **/
void interactive_unification() {
	Term *term1 = NULL, *term2 = NULL;
	Substitution *mgu;
	while(1) {
		while(term1 == NULL) {
			printf("unify> ");
			term1 = parser_term(stdin);
		}
		while(term2 == NULL) {
			printf("unify(");
			term_print(term1);
			printf(")> ");
			term2 = parser_term(stdin);
		}
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