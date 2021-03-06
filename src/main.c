/*H*
 * 
 * FILENAME: main.c
 * DESCRIPTION: Main file
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 16.11.2019
 * COMPILING: gcc -I/usr/include -L *.h *.c -o herbrand -lm -g
 * 
 *H*/

#include "main.h"



/**
  * 
  * This function initilizes the interpreter.
  * 
  **/
int main(int argc, char *argv[]) {
	Program *program;
	FILE *file;
	char *locale = setlocale(LC_ALL, "");
	if(argc == 2) {
		file = fopen(argv[1], "r");
		if(file != NULL) {
			program = program_init();
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
  **/
void interactive_query() {
	Program *program;
	FILE *file;
	Derivation *D;
	Substitution *answer;
	char c, d;
	Term *term = NULL;
	program = program_init();
	while(1) {
		while(term == NULL) {
			printf("\x1b[1m\x1b[31mherbrand>\x1b[0m ");
			term = parser_term(stdin);
		}
		if(term->type == TYPE_ATOM && wcscmp(term->term.string, L"exit") == 0) {
			term_free(term);
			break;
		}
		D = semantics_query(term);
		do {
			answer = semantics_answer(program, D);
			substitution_print(answer);
			if(answer != NULL)
				substitution_free(answer);
			if(D->nb_states == 0) {
				printf(".\n");
				break;
			}
			printf(" ");
			c = getchar();
			if(c != '\n')
				while((d = getchar()) != '\n' && d != EOF);
		} while(answer != NULL && c == ';');
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

/**
  * 
  * This function creates a program with all information
  * about builtin predicates, returning a pointer to a
  * newly initialized Program struct.
  * 
  **/
Program *program_init() {
	Program *program;
	FILE *file;
	wchar_t *path;
	char *c_path;
	int size;
	size = wcslen(HERBRAND_PATH) + wcslen(L"modules/builtin.hb") + 1;
	program = program_alloc();
	path = malloc(sizeof(wchar_t) * size);
	c_path = malloc(sizeof(char) * size);
	swprintf(path, size, HERBRAND_PATH L"modules/builtin.hb");
	wcstombs(c_path, path, size);
	file = fopen(c_path, "r");
	free(path);
	free(c_path);
	if(file != NULL) {
		parser_stream(program, file);
		fclose(file);
	}
	return program;
}