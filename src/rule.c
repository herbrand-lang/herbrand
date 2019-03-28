/*H*
 * 
 * FILENAME: rule.c
 * DESCRIPTION: Data structures and functions for storing and manipuling rules
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 28.03.2019
 * 
 *H*/

#include "rule.h"



/**
  * 
  * This function creates a rule returning a pointer
  * to a newly initialized Rule struct.
  * 
  **/
Rule *rule_alloc(int dynamic, int determinist) {
	Rule *rule = malloc(sizeof(Rule));
	rule->name = NULL;
	rule->type = NULL;
	rule->nb_clauses = 0;
	rule->max_clauses = dynamic ? N_DYN_CLAUSES : N_CLAUSES;
	rule->clauses = malloc(sizeof(Clause*)*rule->max_clauses);
	rule->dynamic = dynamic;
	rule->determinist = determinist;
	return rule;
}

/**
  * 
  * This function increases the memory reserved for
  * clauses in a rule. Returns 0 if the request fails,
  * or 1 if it succeeds.
  * 
  **/
int rule_realloc(Rule *rule) {
	rule->max_clauses += rule->dynamic ? N_DYN_CLAUSES : N_CLAUSES;
	rule->clauses = realloc(rule->clauses, sizeof(Clause*)*rule->max_clauses);
	return rule->clauses != NULL;
}

/**
  * 
  * This function frees a previously allocated rule.
  * The clauses and terms underlying the rule will
  * also be deallocated.
  * 
  **/
void rule_free(Rule *rule) {
	int i;
	for(i = 0; i < rule->nb_clauses; i++)
		clause_free(rule->clauses[i]);
	if(rule->clauses != NULL)
		free(rule->clauses);
	if(rule->type != NULL)
		term_free(rule->type);
	if(rule->name != NULL)
		free(rule->name);
	free(rule);
}

/**
  * This function checks if a rule cannot store new
  * clauses.
  * 
  **/
int rule_is_full(Rule *rule) {
	return rule->nb_clauses == rule->max_clauses;
}

/**
  * 
  * This function sets the name of a rule. Returns 0 if
  * the request fails, or 1 if it succeeds.
  * 
  **/
int rule_set_name(Rule *rule, char *name) {
	if(rule->name != NULL)
		free(rule->name);
	rule->name = malloc(sizeof(char)*(strlen(name)+1));
	strcpy(rule->name, name);
	return name != NULL;
}

/**
  * 
  * This function adds a new clause to a rule. If the
  * memory of the rule is full, the function resizes
  * it. Returns 0 if the request fails, or 1 if it succeeds.
  * 
  **/
int rule_add_clause(Rule *rule, Clause *clause) {
	if(rule_is_full(rule))
		if(rule_realloc(rule) == 0)
			return 0;
	rule->clauses[rule->nb_clauses] = clause;
	rule->nb_clauses++;
	return 1;
}

/**
  * 
  * This function prints for the standard output
  * the whole rule.
  * 
  **/
void rule_print(Rule *rule) {
	int i;
	printf(rule->dynamic ? "#dynamic " : "#static ");
	printf(rule->determinist ? "#det " : "#nondet ");
	printf("\n");
	printf("(predicate %s %d ", rule->name, rule->arity);
	term_print(rule->type);
	printf("\n");
	for(i = 0; i < rule->nb_clauses; i++) {
		printf("\t");
		clause_print(rule->clauses[i]);
		if(i+1 == rule->nb_clauses)
			printf(")");
		printf("\n");
	}
}