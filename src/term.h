/*H*
 * 
 * FILENAME: term.h
 * DESCRIPTION: Data structures and functions for storing and manipuling terms
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 27.01.2019
 * 
 *H*/

#include <stdio.h>
#include <stdlib.h>



#ifndef LOGIC_TERM_H
#define LOGIC_TERM_H

typedef enum {TYPE_ATOM, TYPE_VARIABLE, TYPE_NUMERAL, TYPE_DECIMAL, TYPE_STRING, TYPE_LIST} Type;

typedef struct Term {
	union {
		int numeral;
		double decimal;
		char *string;
		struct List *list;
	} term;
	Type type;
} Term;

typedef struct List {
	struct Term *head;
	struct Term *tail;
} List;

#endif



/**
  * 
  * This function frees a previously allocated term.
  * 
  **/
void term_free(Term *term);

/**
  * 
  * This function prints for the standard output a term.
  * 
  **/
void term_print(Term *term);