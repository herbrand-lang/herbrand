/*H*
 * 
 * FILENAME: term.c
 * DESCRIPTION: Data structures and functions for storing and manipuling terms
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 27.01.2019
 * 
 *H*/

#include "term.h"



/**
  * 
  * This function frees a previously allocated term.
  * 
  **/
void term_free(Term *term) {
	switch(term->type) {
		case TYPE_ATOM:
		case TYPE_STRING:
		case TYPE_VARIABLE:
			free(term->term.string);
			break;
		case TYPE_LIST:
			if(term->term.list->head != NULL)
				term_free(term->term.list->head);
			if(term->term.list->tail != NULL)
				term_free(term->term.list->tail);
			free(term->term.list);
			break;
	}
	free(term);
}

/**
  * 
  * This function prints for the standard output a term.
  * 
  **/
void term_print(Term *term) {
	int length = 0;
	Term *list;
	if(term == NULL)
		return;
	switch(term->type) {
		case TYPE_ATOM:
		case TYPE_VARIABLE:
			printf("%s", term->term.string);
			break;
		case TYPE_NUMERAL:
			printf("%d", term->term.numeral);
			break;
		case TYPE_DECIMAL:
			printf("%f", term->term.decimal);
			break;
		case TYPE_STRING:
			printf("\"%s\"", term->term.string);
			break;
		case TYPE_LIST:
			printf("(");
			list = term;
			while(list->type == TYPE_LIST && list->term.list->head != NULL) {
				if(length)
					printf(" ");
				length++;
				term_print(list->term.list->head);
				list = list->term.list->tail;
			}
			if(list->type != TYPE_LIST) {
				printf("|");
				term_print(list);
			}
			printf(")");
			break;
	}
}