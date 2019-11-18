/*H*
 * 
 * FILENAME: clause.h
 * DESCRIPTION: Data structures and functions for storing and manipuling clauses
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 18.11.2019
 * 
 *H*/

#ifndef HERBRAND_CLAUSE_H
#define HERBRAND_CLAUSE_H

#include <stdio.h>
#include <stdlib.h>
#include "term.h"
#include "hashmap.h"

typedef struct Clause {
	Term *head;
	Term *body;
} Clause;

/**
  * 
  * This function creates a clause, returning a pointer
  * to a newly initialized Clause struct.
  * 
  **/
Clause *clause_alloc();

/**
  * 
  * This function frees a previously allocated clause.
  * The terms underlying the clause will also be deallocated.
  * 
  **/
void clause_free(Clause *clause);

/**
  * 
  * This function renames the variables of a clause.
  * 
  **/
Clause *clause_rename_variables(Clause *clause, int *id);

/**
  * 
  * This function prints for the standard output
  * the whole clause.
  * 
  **/
void clause_print(Clause *clause);

#endif