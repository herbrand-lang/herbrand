/*H*
 * 
 * FILENAME: program.c
 * DESCRIPTION: Data structures and functions for storing and manipuling substitutions
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 03.04.2019
 * 
 *H*/

#include <stdio.h>
#include <stdlib.h>
#include "term.h"
#include "hashmap.h"



#ifndef LOGIC_SUBSTITUTION_H
#define LOGIC_SUBSTITUTION_H

typedef struct Substitution {
	wchar_t **domain;
	Term **range;
	Hashmap *indices;
  int max_vars;
	int nb_vars;
  int references;
} Substitution;

#endif



/**
  * 
  * Identity substitution whitout any variable.
  * 
  **/
Substitution LOGIC_SUBSTITUTION_ID;

/**
  * 
  * This function creates a substitution, returning a pointer
  * to a newly initialized Substitution struct.
  * 
  **/
Substitution *substitution_alloc(int nb_vars);

/**
  * 
  * This function creates a substitution from a term,
	* returning a pointer to a newly initialized Substitution struct.
  * 
  **/
Substitution *substitution_alloc_from_term(Term *term);

/**
  * 
  * This function frees a previously allocated substitution.
  * The terms, strings and hashmap underlying the substitution
  * will also be deallocated.
  * 
  **/
void substitution_free(Substitution *subs);

/**
  * 
  * This function increases in one the number
  * of references to a substitution.
  * 
  **/
void substitution_increase_references(Substitution *subs);

/**
  * 
  * This function adds a new link into a substitution.
  * Returns 0 if the request fails, or 1 if it succeeds.
  * 
  **/
int substitution_add_link(Substitution *subs, wchar_t *var, Term *value);

/**
  * 
  * This function gets the term linked with a variable
  * from a substitution. If variable is not contained 
  * in the substitution, returns NULL.
  * 
  **/
Term *substitution_get_link(Substitution *subs, wchar_t *var);

/**
	*
	* This function composes two substitutions. This
	* function modifies the original first substitution.
	* 
	**/
Substitution *substitution_compose(Substitution *u, Substitution *v, int join);

/**
  *
  * This function applies a substitution to a term.
  * 
  **/
Term *term_apply_substitution(Term *term, Substitution *subs);

/**
  * 
  * This function prints for the standard output a substitution.
  * 
  **/
void substitution_print(Substitution *subs);