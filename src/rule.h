/*H*
 * 
 * FILENAME: rule.h
 * DESCRIPTION: Data structures and functions for storing and manipuling rules
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 18.11.2019
 * 
 *H*/

#ifndef HERBRAND_RULE_H
#define HERBRAND_RULE_H

#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>
#include "clause.h"
#include "term.h"

#define N_CLAUSES 1
#define N_DYN_CLAUSES 32

typedef struct Rule {
	Clause **clauses;
	Term *type;
	wchar_t *name;
	int arity;
	int nb_clauses;
	int max_clauses;
	int dynamic;
	int determinist;
	int local;
  int tail_recursive;
} Rule;

/**
  * 
  * This function creates a rule returning a pointer
  * to a newly initialized Rule struct.
  * 
  **/
Rule *rule_alloc(int dynamic, int determinist, int local);

/**
  * 
  * This function increases the memory reserved for
  * clauses in a rule. Returns 0 if the request fails,
  * or 1 if it succeeds.
  * 
  **/
int rule_realloc(Rule *rule);

/**
  * 
  * This function frees a previously allocated rule.
  * The clauses and terms underlying the rule will
  * also be deallocated.
  * 
  **/
void rule_free(Rule *rule);

/**
  * This function checks if a rule cannot store new
  * clauses.
  * 
  **/
int rule_is_full(Rule *rule);

/**
  * 
  * This function sets the name of a rule. Returns 0 if
  * the request fails, or 1 if it succeeds.
  * 
  **/
int rule_set_name(Rule *rule, wchar_t *name);

/**
  * 
  * This function adds a new clause to a rule. If the
  * memory of the rule is full, the function resizes
  * it. Returns 0 if the request fails, or 1 if it succeeds.
  * 
  **/
int rule_add_clause(Rule *rule, Clause *clause);

/**
  * 
  * This function prints for the standard output
  * the whole rule.
  * 
  **/
void rule_print(Rule *rule);

#endif