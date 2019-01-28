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
  * 
  * This function frees a previously allocated program.
  * The rules, clauses and terms underlying the program
  * will also be deallocated.
  * 
  **/
void program_free(Program *program) {
	int i;
	for(i = 0; i < program->nb_rules; i++)
		rule_free(program->rules[i]);
	hashmap_free(program->indices);
	free(program->rules);
	free(program);
}

/**
  *
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
	int i;
	for(i = 0; i < program->nb_rules; i++) {
		printf("%s/%d :: ", program->rules[i]->name, program->rules[i]->arity);
		term_print(program->rules[i]->type);
		printf(program->rules[i]->dynamic ? " #dynamic" : " #static");
		printf(program->rules[i]->determinist ? " #det" : " #nondet");
		printf("\n");
	}
}

/**
  * 
  * This function prints for the standard output
  * the whole program.
  * 
  **/
void program_print(Program *program) {
	int i;
	for(i = 0; i < program->nb_rules; i++) {
		rule_print(program->rules[i]);
	}
}