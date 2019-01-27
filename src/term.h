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
  * This function creates an empty list, returning a
  * pointer to a newly initialized Term struct.
  * 
  **/
Term *term_list_empty();

/**
  * 
  * This function adds an element to a list, returning
  * the pointer to the last element inserted in the struct.
  * 
  **/
Term *term_list_add_element(Term *list, Term *term);

/**
  * 
  * This function adds an element as a tail list. The 
  * tail underlying the list will be deallocated.
  * 
  **/
Term *term_list_set_tail(Term *list, Term *term);

/**
  * 
  * This function prints for the standard output a term.
  * 
  **/
void term_print(Term *term);