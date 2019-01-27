/*H*
 * 
 * FILENAME: structures.h
 * DESCRIPTION: Operations with data structures
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 25.01.2019
 * 
 *H*/

#include "structures.h"



/**
  * 
  * This function ...
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
  * This function ...
  * 
  **/
int program_realloc(Program *program) {
	program->max_rules += N_RULES;
	program->rules = realloc(program->rules, sizeof(Rule*)*program->max_rules);
	return program->rules != NULL;
}

/**
  * 
  * This function ...
  * 
  **/
int program_is_full(Program *program) {
	return program->nb_rules == program->max_rules;
}

/**
  * 
  * This function ...
  * 
  **/
int program_add_rule(Program *program, char *name, int arity, int nb_clauses, Term **type, Clause **clauses) {
	Rule *rule = rule_alloc(strlen(name), arity, nb_clauses);
	hashmap_append(program->indices, name, program->nb_rules);
	program->rules[program->nb_rules] = rule;
	strcpy(rule->name, name);
	rule->arity = arity;
	rule->type = type;
	rule->clauses = clauses;
	rule->nb_clauses = nb_clauses;
	program->nb_rules++;
	if(program_is_full(program))
		return program_realloc(program);
	return 1;
}

/**
  * 
  * This function ...
  * 
  **/
Rule *rule_alloc(int name_length, int arity, int nb_clauses) {
	Rule *rule = malloc(sizeof(Rule));
	rule->name = malloc(sizeof(char)*(name_length+1));
	rule->type = malloc(sizeof(Term*)*arity);
	rule->clauses = malloc(sizeof(Clause*)*nb_clauses);
	return rule;
}

/**
  * 
  * This function creates a clause with $nb_args arguments in the head
	* and $nb_atoms atoms in the body, returning a pointer to a newly
	* initialized Clause struct.
  * 
  **/
Clause *clause_alloc(int nb_args, int nb_atoms) {
	Clause *clause = malloc(sizeof(Clause*));
	clause->head = malloc(sizeof(Term*)*nb_args);
	clause->body = malloc(sizeof(Term*)*nb_atoms);
	clause->nb_args = nb_args;
	clause->nb_atoms = nb_atoms;
	return clause;
}

/**
  * 
  * This function ...
  * 
  **/
void program_listing(Program *program) {
	int i, j;
	for(i = 0; i < program->nb_rules; i++) {
		printf("%s/%d :: (", program->rules[i]->name, program->rules[i]->arity);
		for(j = 0; j < program->rules[i]->arity; j++) {
			if(j > 0) printf(" ");
			term_print(program->rules[i]->type[j]);
		}
		printf(")\n");
	}
}

/**
  * 
  * This function ...
  * 
  **/
void term_print(Term *term) {
	int length = 0;
	Term *list;
	if(term == NULL)
		return;
	switch(term->type) {
		case ATOM:
		case VARIABLE:
			printf("%s", term->term.string);
			break;
		case NUMERAL:
			printf("%d", term->term.numeral);
			break;
		case DECIMAL:
			printf("%f", term->term.decimal);
			break;
		case STRING:
			printf("\"%s\"", term->term.string);
			break;
		case LIST:
			printf("(");
			list = term;
			while(list->type == LIST && list->term.list->head != NULL) {
				if(length)
					printf(" ");
				length++;
				term_print(list->term.list->head);
				list = list->term.list->tail;
			}
			if(list->type != LIST) {
				printf("|");
				term_print(list);
			}
			printf(")");
			break;
	}
}