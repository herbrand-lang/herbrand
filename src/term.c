/*H*
 * 
 * FILENAME: term.c
 * DESCRIPTION: Data structures and functions for storing and manipuling terms
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 28.03.2019
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
  * This function renames the variables of a term.
  * 
  **/
Term *term_rename_variables(Term *term, int *id, Hashmap *vars) {
	int index, mod, length = 2;
	Term *var;
	if(term == NULL)
		return NULL;
	else if(term->type == TYPE_VARIABLE) {
		index = hashmap_lookup(vars, term->term.string);
		var = malloc(sizeof(Term));
		var->type = TYPE_VARIABLE;
		if(index != -1) {
			mod = index;
			while(mod != 0) {
				mod /= 10;
				length++;
			}
			var->term.string = malloc(sizeof(char)*length);
			sprintf(var->term.string, "$%d", index);
			return var;
		} else {
			(*id)++;
			mod = *id;
			while(mod != 0) {
				mod /= 10;
				length++;
			}
			var->term.string = malloc(sizeof(char)*length);
			hashmap_append(vars, term->term.string, *id);
			sprintf(var->term.string, "$%d", *id);
		}
		return var;
	} else if(term->type == TYPE_LIST && !term_list_is_null(term)) {
		return term_list_create(
			term_rename_variables(term->term.list->head, id, vars),
			term_rename_variables(term->term.list->tail, id, vars));
	} else
		return term;
}

/**
  * 
  * This function creates a list, returning a
  * pointer to a newly initialized Term struct.
  * 
  **/
Term *term_list_create(Term *head, Term *tail) {
		Term *list = malloc(sizeof(Term));
		list->type = TYPE_LIST;
		list->term.list = malloc(sizeof(List));
		list->term.list->head = head;
		list->term.list->tail = tail;
		return list;
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
  * This function returns the list of variables
	* contained in the term.
  * 
  **/
Term **term_get_variables(Term *term, int *nb_vars) {
	Term **vars_head, **vars_tail, **vars;
	int i, nb_vars_head, nb_vars_tail;
	if(term->type == TYPE_VARIABLE && strcmp(term->term.string, "_") != 0) {
		(*nb_vars)++;
		vars = malloc(sizeof(Term*));
		vars[0] = term;
		return vars;
	} else if(term->type == TYPE_LIST && !term_list_is_null(term)) {
		nb_vars_head = 0;
		nb_vars_tail = 0;
		vars_head = term_get_variables(term_list_get_head(term), &nb_vars_head);
		vars_tail = term_get_variables(term_list_get_tail(term), &nb_vars_tail);
		(*nb_vars) = nb_vars_head + nb_vars_tail;
		vars = malloc(sizeof(Term*)*(*nb_vars));
		for(i = 0; i < nb_vars_head; i++)
			vars[i] = vars_head[i];
		for(i = 0; i < nb_vars_tail; i++)
			vars[nb_vars_head+i] = vars_tail[i];
		free(vars_head);
		free(vars_tail);
		return vars;
	} else {
		return NULL;
	}
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