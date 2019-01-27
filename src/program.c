/*H*
 * 
 * FILENAME: program.c
 * DESCRIPTION: Data structures and functions for storing and manipuling programs
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 27.01.2019
 * 
 *H*/

#include "program.h"



/**
  * 
  * This function creates a program returning a pointer
  * to a newly initialized Program struct.
  * 
  **/
Program *program_alloc() {
	Program *program = malloc(sizeof(Program));
	program->rules = malloc(sizeof(Rule*)*N_RULES);
	program->indices = hashmap_alloc();
	program->nb_rules = 0;
	program->max_rules = N_RULES;
	return program;
}

/**
  * 
  * This function increases the memory reserved for
  * rules in a program. Returns 0 if the request fails,
  * or 1 if it succeeds.
  * 
  **/
int program_realloc(Program *program) {
	program->max_rules += N_RULES;
	program->rules = realloc(program->rules, sizeof(Rule*)*program->max_rules);
	return program->rules != NULL;
}

/**
  * This function checks if a program cannot store new
  * rules.
  * 
  **/
int program_is_full(Program *program) {
	return program->nb_rules == program->max_rules;
}

/**
  * 
  * This function adds a new rule to a program. If the
  * memory of the program is full, the function resizes
  * it. Returns 0 if the request fails, or 1 if it succeeds.
  * 
  **/
int program_add_rule(Program *program, Rule *rule) {
    if(program_is_full(program))
		if(program_realloc(program) == 0)
            return 0;
	hashmap_append(program->indices, rule->name, program->nb_rules);
	program->rules[program->nb_rules] = rule;
	program->nb_rules++;
	return 1;
}

/**
  * 
  * This function prints for the standard output
  * the list of predicates stored in a program, with
  * the format "name/arity :: type".
  * 
  **/
void program_listing(Program *program) {
	int i, j;
	for(i = 0; i < program->nb_rules; i++) {
		printf("%s/%d :: (", program->rules[i]->name, program->rules[i]->arity);
		for(j = 0; j < program->rules[i]->arity; j++) {
			if(j > 0) printf(" ");
			term_print(&program->rules[i]->type[j]);
		}
		printf(")\n");
	}
}

/**
  * 
  * This function prints for the standard output
  * the whole program.
  * 
  **/
void program_print(Program *program) {
	int i, j, k;
	for(i = 0; i < program->nb_rules; i++) {
		printf("(predicate %s ", program->rules[i]->name);
		term_print(program->rules[i]->type);
		printf("\n");
		for(j = 0; j < program->rules[i]->nb_clauses; j++) {
			printf("\t(");
			term_print(program->rules[i]->clauses[j]->head);
			term_print(program->rules[i]->clauses[j]->body);
			printf(")");
			if(j == program->rules[i]->nb_clauses-1)
				printf(")");
			printf("\n");
		}
	}
}