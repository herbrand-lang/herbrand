/*H*
 * 
 * FILENAME: clause.c
 * DESCRIPTION: Data structures and functions for storing and manipuling clauses
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 28.03.2019
 * 
 *H*/

#include "clause.h"



/**
  * 
  * This function creates a clause, returning a pointer
  * to a newly initialized Clause struct.
  * 
  **/
Clause *clause_alloc() {
	Clause *clause = malloc(sizeof(Clause));
	clause->head = NULL;
	clause->body = NULL;
	return clause;
}

/**
  * 
  * This function frees a previously allocated clause.
  * The terms underlying the clause will also be deallocated.
  * 
  **/
void clause_free(Clause *clause) {
	if(clause->head != NULL)
		term_free(clause->head);
	if(clause->body != NULL)
		term_free(clause->body);
	free(clause);
}

/**
  * 
  * This function renames the variables of a clause.
  * 
  **/
Clause *clause_rename_variables(Clause *clause) {
	int id = 0;
	Clause *renamed = clause_alloc();
	Hashmap *vars = hashmap_alloc(8);
	renamed->head = term_rename_variables(clause->head, &id, vars);
	renamed->body = term_rename_variables(clause->body, &id, vars);
	hashmap_free(vars);
	return renamed;
}

/**
  * 
  * This function prints for the standard output
  * the whole clause.
  * 
  **/
void clause_print(Clause *clause) {
	int i = 0;
	Term *term;
	printf("(");
	term = clause->head;
	while(term->term.list->head != NULL) {
		if(i != 0) printf(" ");
		term_print(term->term.list->head);
		term = term->term.list->tail;
		i++;
	}
	term = clause->body;
	while(term->term.list->head != NULL) {
		printf(" ");
		term_print(term->term.list->head);
		term = term->term.list->tail;
	}
	printf(")");
}