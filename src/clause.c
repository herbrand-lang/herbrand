/*H*
 * 
 * FILENAME: clause.c
 * DESCRIPTION: Data structures and functions for storing and manipuling clauses
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 27.01.2019
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
    term_free(clause->head);
    term_free(clause->body);
    free(clause);
}