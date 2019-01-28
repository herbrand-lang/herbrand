/*H*
 * 
 * FILENAME: program.c
 * DESCRIPTION: Data structures and functions for storing and manipuling substitutions
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 28.01.2019
 * 
 *H*/

#include "substitution.h"



/**
  * 
  * This function creates a substitution, returning a pointer
  * to a newly initialized Substitution struct.
  * 
  **/
Substitution *substitution_alloc(int nb_vars) {
	Substitution *subs = malloc(sizeof(Substitution));
	subs->domain = malloc(sizeof(Term)*nb_vars);
	subs->range = malloc(sizeof(Term)*nb_vars);
	return subs;
}