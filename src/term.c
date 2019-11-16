/*H*
 * 
 * FILENAME: term.c
 * DESCRIPTION: Data structures and functions for storing and manipuling terms
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 16.11.2019
 * 
 *H*/

#include "term.h"



/**
  * 
  * This function creates a term returning a pointer
  * to a newly initialized Term struct.
  * 
  **/
Term *term_alloc() {
	Term *term = malloc(sizeof(Term));
	term->references = 0;
	term->parent = NULL;
	return term;
}

/**
  * 
  * This function frees a previously allocated term.
  * 
  **/
void term_free(Term *term) {
	if(term->references > 0) {
		term->references--;
		return;
	}
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
  * This function initializes an atom returning a pointer
  * to a newly initialized Term struct.
  * 
  **/
Term *term_init_atom(wchar_t *atom) {
	Term *term = term_alloc();
	term->type = TYPE_ATOM;
	term_set_string(term, atom);
	return term;
}

/**
  * 
  * This function initializes a variable returning a pointer
  * to a newly initialized Term struct.
  * 
  **/
Term *term_init_variable(wchar_t *var) {
	Term *term = term_alloc();
	term->type = TYPE_VARIABLE;
	term_set_string(term, var);
	return term;
}

/**
  * 
  * This function initializes a numeral returning a pointer
  * to a newly initialized Term struct.
  * 
  **/
Term *term_init_numeral(long long int numeral) {
	Term *term = term_alloc();
	term->type = TYPE_NUMERAL;
	term->term.numeral = numeral;
	return term;
}

/**
  * 
  * This function initializes a decimal returning a pointer
  * to a newly initialized Term struct.
  * 
  **/
Term *term_init_decimal(long double decimal) {
	Term *term = term_alloc();
	term->type = TYPE_DECIMAL;
	term->term.decimal = decimal;
	return term;
}

/**
  * 
  * This function initializes an string returning a
  * pointer to a newly initialized Term struct.
  * 
  **/
Term *term_init_string(wchar_t *string) {
	Term *term = term_alloc();
	term->type = TYPE_STRING;
	term_set_string(term, string);
	return term;
}

/**
  * 
  * This function initializes a character returning a
  * pointer to a newly initialized Term struct.
  * 
  **/
Term *term_init_char(wchar_t character) {
	Term *term = term_alloc();
	term->type = TYPE_CHAR;
	term->term.character = character;
	return term;
}

/**
  * 
  * This function sets the string of the term.
  * 
  **/
void term_set_string(Term *term, wchar_t *str) {
	term->term.string = malloc(sizeof(wchar_t)*(wcslen(str)+1));
	wcscpy(term->term.string, str);
}

/**
  * 
  * This function increases in one the number
  * of references to a term.
  * 
  **/
void term_increase_references(Term *term) {
	term->references++;
}

/**
  * 
  * This function checks if term is callable.
  * 
  **/
int term_is_callable(Term *term) {
	if(term->type != TYPE_LIST)
		return 0;
	else if(term_list_is_null(term))
		return 1;
	else if(term->term.list->head->type != TYPE_ATOM)
		return 0;
	while(term->type == TYPE_LIST && !term_list_is_null(term))
		term = term->term.list->tail;
	return term_list_is_null(term);
}

/**
  * 
  * This function checks if term is evaluable.
  * 
  **/
int term_is_evaluable(Term *term) {
	if(term->type != TYPE_LIST || term_list_is_null(term))
		return 0;
	return term->term.list->head->type == TYPE_ATOM;
}

/**
  * 
  * This function checks if term is variable.
  * 
  **/
int term_is_variable(Term *term) {
	return term->type == TYPE_VARIABLE;
}

/**
  * 
  * This function checks if term is an atom.
  * 
  **/
int term_is_atom(Term *term) {
	return term->type == TYPE_ATOM;
}

/**
  * 
  * This function checks if term is a string.
  * 
  **/
int term_is_string(Term *term) {
	return term->type == TYPE_STRING;
}

/**
  * 
  * This function checks if term is a number.
  * 
  **/
int term_is_number(Term *term) {
	return term->type == TYPE_NUMERAL || term->type == TYPE_DECIMAL;
}

/**
  * 
  * This function checks if term is a character.
  * 
  **/
int term_is_char(Term *term) {
	return term->type == TYPE_CHAR;
}

/**
  * 
  * This function checks if term is a integer.
  * 
  **/
int term_is_integer(Term *term) {
	return term->type == TYPE_NUMERAL;
}

/**
  * 
  * This function checks if term is a float.
  * 
  **/
int term_is_float(Term *term) {
	return term->type == TYPE_DECIMAL;
}

/**
  * 
  * This function checks if term is a list.
  * 
  **/
int term_is_list(Term *term) {
	return term->type == TYPE_LIST;
}

/**
  * 
  * This function checks if term is a catcher.
  * 
  **/
int term_is_catcher(Term *term) {
	return term_is_callable(term) && !term_list_is_null(term)
		&& wcscmp(term_list_get_head(term)->term.string, L"$catcher") == 0;
}

/**
  * 
  * This function compares two terms.
  * 
  **/
int term_compare(Term *term1, Term *term2) {
	int i, cmp;
	wchar_t char1, char2;
	Term *term3;
	if(term1->type == TYPE_STRING && term_list_is_string(term2)
	|| term2->type == TYPE_STRING && term_list_is_string(term1)) {
		cmp = 1;
		if(term1->type == TYPE_STRING) {
			term3 = term1;
			term1 = term2;
			term2 = term3;
			cmp = -1;
		}
		i = 0;
		while(term1->type == TYPE_LIST && !term_list_is_null(term1)) {
			char1 = term1->term.list->head->term.character;
			char2 = term2->term.string[i];
			if(char1 < char2)
				return -1*cmp;
			else if(char1 > char2)
				return 1*cmp;
			term1 = term1->term.list->tail;
			i++;
		}
		if(term_list_is_null(term1))
			return wcscmp(L"", term2->term.string+i)*cmp;
		else
			return wcscmp(term1->term.string, term2->term.string+i)*cmp;
	}
	if(term1->type != term2->type) {
		if(term1->type < term2->type)
			return -1;
		else
			return 1;
	}
	switch(term1->type) {
		case TYPE_VARIABLE:
		case TYPE_STRING:
		case TYPE_ATOM:
			return wcscmp(term1->term.string, term2->term.string);
			break;
		case TYPE_CHAR:
			if(term1->term.character < term2->term.character)
				return -1;
			else if(term1->term.character > term2->term.character)
				return 1;
			else
				return 0;
			break;
		case TYPE_NUMERAL:
			if(term1->term.numeral < term2->term.numeral)
				return -1;
			else if(term1->term.numeral > term2->term.numeral)
				return 1;
			else
				return 0;
			break;
		case TYPE_DECIMAL:
			if(term1->term.decimal < term2->term.decimal)
				return -1;
			else if(term1->term.decimal > term2->term.decimal)
				return 1;
			else
				return 0;
			break;
		case TYPE_LIST:
			cmp = 0;
			if(term_list_is_null(term1) && term_list_is_null(term2))
				return 0;
			while(term1->type == TYPE_LIST && term2->type == TYPE_LIST
			&& !term_list_is_null(term1) && !term_list_is_null(term2)) {
				cmp = term_compare(term1->term.list->head, term2->term.list->head);
				if(cmp != 0)
					return cmp;
				term1 = term1->term.list->tail;
				term2 = term2->term.list->tail;
			}
			return term_compare(term1, term2);
			break;
	}
	return 0;
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
		var = term_alloc();
		var->type = TYPE_VARIABLE;
		if(index != -1) {
			mod = index;
			while(mod != 0) {
				mod /= 10;
				length++;
			}
			var->term.string = malloc(sizeof(wchar_t)*length);
			swprintf(var->term.string, length+2, L"$%d", index);
		} else {
			(*id)++;
			mod = *id;
			while(mod != 0) {
				mod /= 10;
				length++;
			}
			var->term.string = malloc(sizeof(wchar_t)*length);
			hashmap_append(vars, term->term.string, *id);
			swprintf(var->term.string, length+2, L"$%d", *id);
		}
		return var;
	} else if(term->type == TYPE_LIST && !term_list_is_null(term)) {
		var = term_list_create(
			term_rename_variables(term->term.list->head, id, vars),
			term_rename_variables(term->term.list->tail, id, vars));
		var->parent = term->parent;
		return var;
	} else {
		term_increase_references(term);
		return term;
	}
}

/**
  * 
  * This function returns the length of a list term.
  * If term is not a well-formed list, returns -1.
  * 
  **/
int term_list_length(Term *list) {
	int length = 0;
	while(list->type == TYPE_LIST && !term_list_is_null(list)) {
		length++;
		list = list->term.list->tail;
	}
	return list->type == TYPE_LIST ? length : -1;
}

/**
  * 
  * This function creates a list, returning a
  * pointer to a newly initialized Term struct.
  * 
  **/
Term *term_list_create(Term *head, Term *tail) {
	Term *list = term_alloc();
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
	Term *list = term_alloc();
	list->type = TYPE_LIST;
	list->term.list = malloc(sizeof(List));
	list->term.list->head = NULL;
	list->term.list->tail = NULL;
	return list;
}

/**
  * 
  * This function clones a list, returning a
  * pointer to a newly initialized Term struct.
  * 
  **/
Term *term_list_clone(Term *term) {
	if(term_list_is_null(term))
		return term_list_empty();
	term_increase_references(term->term.list->head);
	return term_list_create(
		term->term.list->head,
		term_list_clone(term->term.list->tail));
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
  * This function checks whether a list represents
  * an string.
  * 
  **/
int term_list_is_string(Term *term) {
	while(term->type == TYPE_LIST && !term_list_is_null(term)) {
		if(term->term.list->head->type != TYPE_CHAR)
			return 0;
		term = term->term.list->tail;
	}
	return term_list_is_null(term) || term->type == TYPE_STRING;
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
  * This function returns the nth-elemnt of a list.
  * 
  **/
Term *term_list_get_nth(Term *term, int index) {
	while(index > 0 && term->type == TYPE_LIST) {
		term = term->term.list->tail;
		index--;
	}
	return index ? NULL : term->term.list->head;
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
	if(term->type == TYPE_VARIABLE && wcscmp(term->term.string, L"_") != 0) {
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
  * This function returns the list of catchers
  * contained in the term.
  * 
  **/
void **term_get_catchers(Term *term, int *nb_catchers) {
	void **catchers_head, **catchers_tail, **catchers;
	int i, nb_catchers_head, nb_catchers_tail;
	if(term->type == TYPE_LIST && !term_list_is_null(term)) {
		if(term_is_catcher(term)) {
			(*nb_catchers)++;
			catchers = malloc(sizeof(void*));
			catchers[0] = term_list_get_head(term)->parent;
			return catchers;
		} else {
			nb_catchers_head = 0;
			nb_catchers_tail = 0;
			catchers_head = term_get_catchers(term_list_get_head(term), &nb_catchers_head);
			catchers_tail = term_get_catchers(term_list_get_tail(term), &nb_catchers_tail);
			(*nb_catchers) = nb_catchers_head + nb_catchers_tail;
			catchers = malloc(sizeof(void*)*(*nb_catchers));
			for(i = 0; i < nb_catchers_head; i++)
				catchers[i] = catchers_head[i];
			for(i = 0; i < nb_catchers_tail; i++)
				catchers[nb_catchers_head+i] = catchers_tail[i];
			free(catchers_head);
			free(catchers_tail);
			return catchers;
		}
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
			printf("\x1b[1m\x1b[34m%ls\x1b[0m", term->term.string);
			break;
		case TYPE_VARIABLE:
			printf("\x1b[1m\x1b[36m%ls\x1b[0m", term->term.string);
			break;
		case TYPE_NUMERAL:
			printf("\x1b[1m\x1b[33m%lli\x1b[0m", term->term.numeral);
			break;
		case TYPE_DECIMAL:
			printf("\x1b[1m\x1b[33m%Lf\x1b[0m", term->term.decimal);
			break;
		case TYPE_CHAR:
			printf("\x1b[1m\x1b[32m'%lc'\x1b[0m", term->term.character);
			break;
		case TYPE_STRING:
			printf("\x1b[1m\x1b[32m\"%ls\"\x1b[0m", term->term.string);
			break;
		case TYPE_LIST:
			if(term_list_is_string(term)) {
				printf("\x1b[1m\x1b[32m\"");
				while(term->type == TYPE_LIST && !term_list_is_null(term)) {
					printf("%lc", term->term.list->head->term.character);
					term = term->term.list->tail;
				}
				if(term->type == TYPE_STRING)
					printf("%ls", term->term.string);
				printf("\"\x1b[0m");
				break;
			}
			printf("\x1b[1m(\x1b[0m");
			list = term;
			while(list->type == TYPE_LIST && list->term.list->head != NULL) {
				if(length)
					printf(" ");
				length++;
				term_print(list->term.list->head);
				list = list->term.list->tail;
			}
			if(list->type != TYPE_LIST) {
				printf("\x1b[1m|\x1b[0m");
				term_print(list);
			}
			printf("\x1b[1m)\x1b[0m");
			break;
	}
}

/**
  * 
  * This function selects the most left term
  * of the goal in a state.
  * 
  **/
Term *term_select_most_left(Term *term) {
	if(term == NULL || term_list_is_null(term))
		return NULL;
	while(term->type == TYPE_LIST && term->term.list->head->type == TYPE_LIST && !term_list_is_null(term->term.list->head))
		term = term->term.list->head;
	return term;
}

/**
  * 
  * This function replaces the most left term
  * of the goal in a state.
  * 
  **/
Term *term_replace_most_left(Term *term, Term *head) {
	if(term->type == TYPE_LIST && term->term.list->head->type == TYPE_LIST && !term_list_is_null(term->term.list->head)) {
		term_increase_references(term->term.list->tail);
		if(term->term.list->head->type == TYPE_LIST
		&& term->term.list->head->term.list->head->type == TYPE_LIST
		&& !term_list_is_null(term->term.list->head->term.list->tail)) {
			return term_list_create(
				term_replace_most_left(term->term.list->head, head),
				term->term.list->tail);
		} else {
			if(head == NULL || term_list_is_null(head)) {
				return term->term.list->tail;
			} else {
				term_increase_references(head);
				return term_list_create(head, term->term.list->tail);
			}
		}
	} else {
		term_increase_references(head);
		return head;
	}
}

/**
  * 
  * This function finds a term.
  * 
  **/
int term_search_term(Term *term, Term *needle) {
	if(term == needle)
		return 1;
	while(term->type == TYPE_LIST && !term_list_is_null(term)) {
		if(term_search_term(term->term.list->head, needle))
			return 1;
		term = term->term.list->tail;
	}
	return term == needle;
}