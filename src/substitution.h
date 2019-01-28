/*H*
 * 
 * FILENAME: program.c
 * DESCRIPTION: Data structures and functions for storing and manipuling substitutions
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 28.01.2019
 * 
 *H*/

#include <stdio.h>
#include <stdlib.h>
#include "term.h"



#ifndef LOGIC_SUBSTITUTION_H
#define LOGIC_SUBSTITUTION_H

typedef struct Substitution {
	char **domain;
	Term *range;
	int nb_vars;
} Substitution;

#endif



/**
  * 
  * This function creates a substitution, returning a pointer
  * to a newly initialized Substitution struct.
  * 
  **/
Substitution *substitution_alloc(int nb_vars);