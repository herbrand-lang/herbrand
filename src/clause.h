/*H*
 * 
 * FILENAME: clause.h
 * DESCRIPTION: Data structures and functions for storing and manipuling clauses
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 27.01.2019
 * 
 *H*/

#include <stdio.h>
#include <stdlib.h>
#include "term.h"



#ifndef LOGIC_CLAUSE_H
#define LOGIC_CLAUSE_H

typedef struct Clause {
	Term *head;
	Term *body;
} Clause;

#endif



/**
  * 
  * This function creates a clause, returning a pointer
  * to a newly initialized Clause struct.
  * 
  **/
Clause *clause_alloc(int arity);

/**
  * 
  * This function frees a previously allocated clause.
  * The terms underlying the clause will also be deallocated.
  * 
  **/
void clause_free(Clause *clause);