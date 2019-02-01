/*H*
 * 
 * FILENAME: term.c
 * DESCRIPTION: Data structures and functions for storing and manipuling terms
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 01.02.2019
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
  * This function creates an empty list, returning a
  * pointer to a newly initialized Term struct.
  * 
  **/
Term *term_list_empty() {
    Term *list = malloc(sizeof(Term));
    list->type = TYPE_LIST;
    list->term.list = malloc(sizeof(List));
    list->term.list->head = NULL;
    list->term.list->tail = NULL;
    return list;
}

/**
  * 
  * This function checks whether a list is empty.
  * 
  **/
int term_list_is_null(Term *term) {
	return term->type == TYPE_LIST && term->term.list->head == NULL;
}

/**
  * 
  * This function returns the head of a list.
  * 
  **/
Term *term_list_get_head(Term *term) {
	return term->term.list->head;
}

/**
  * 
  * This function returns the tail of a list.
  * 
  **/
Term *term_list_get_tail(Term *term) {
	return term->term.list->tail;
}

/**
  * 
  * This function adds an element to a list, returning
  * the pointer to the last element inserted in the struct.
  * 
  **/
Term *term_list_add_element(Term *list, Term *term) {
    while(list->term.list->head != NULL)
        list = list->term.list->tail;
    list->term.list->head = term;
    list->term.list->tail = term_list_empty();
    return list->term.list->tail;
}

/**
  * 
  * This function adds an element as a tail list. The 
  * tail underlying the list will be deallocated.
  * 
  **/
Term *term_list_set_tail(Term *list, Term *term) {
    Term *prev;
    while(list->term.list->head != NULL) {
        prev = list;
        list = list->term.list->tail;
    }
    term_free(prev->term.list->tail);
    prev->term.list->tail = term;
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