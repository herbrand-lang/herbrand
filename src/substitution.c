/*H*
 * 
 * FILENAME: program.c
 * DESCRIPTION: Data structures and functions for storing and manipuling substitutions
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 06.04.2019
 * 
 *H*/

#include "substitution.h"



/**
  * 
  * Identity substitution whitout any variable.
  * 
  **/
Substitution LOGIC_SUBSTITUTION_ID = {NULL, NULL, NULL, 0, 0};

/**
  * 
  * This function creates a substitution, returning a pointer
  * to a newly initialized Substitution struct.
  * 
  **/
Substitution *substitution_alloc(int nb_vars) {
	Substitution *subs = malloc(sizeof(Substitution));
	subs->domain = malloc(sizeof(wchar_t*)*nb_vars);
	subs->range = malloc(sizeof(Term*)*nb_vars);
	subs->indices = hashmap_alloc(nb_vars);
	subs->nb_vars = 0;
	subs->max_vars = nb_vars;
	subs->references = 0;
	return subs;
}

/**
  * 
  * This function creates a substitution from a term,
	* returning a pointer to a newly initialized Substitution struct.
  * 
  **/
Substitution *substitution_alloc_from_term(Term *term) {
	int i, nb_vars = 0;
	Term **vars = term_get_variables(term, &nb_vars);
	Substitution *subs = malloc(sizeof(Substitution));
	subs->domain = malloc(sizeof(wchar_t*)*nb_vars);
	subs->range = malloc(sizeof(Term*)*nb_vars);
	subs->indices = hashmap_alloc(nb_vars);
	subs->nb_vars = 0;
	subs->max_vars = nb_vars;
	subs->references = 0;
	for(i = 0; i < nb_vars; i++) {
		if(substitution_get_link(subs, vars[i]->term.string) == NULL)
			substitution_add_link(subs, vars[i]->term.string, vars[i]);
	}
	free(vars);
	return subs;
}

/**
  * 
  * This function frees a previously allocated substitution.
  * The terms, strings and hashmap underlying the substitution
  * will also be deallocated.
  * 
  **/
void substitution_free(Substitution *subs) {
	int i;
	if(subs->references == 0) {
		for(i = 0; i < subs->nb_vars; i++) {
			free(subs->domain[i]);
			term_free(subs->range[i]);
		}
		free(subs->domain);
		free(subs->range);
		hashmap_free(subs->indices);
		free(subs);
	} else {
		subs->references--;
	}
}

/**
  * 
  * This function increases in one the number
  * of references to a substitution.
  * 
  **/
void substitution_increase_references(Substitution *subs) {
	subs->references++;
}

/**
  * 
  * This function adds a new link into a substitution.
  * Returns 0 if the request fails, or 1 if it succeeds.
  * 
  **/
int substitution_add_link(Substitution *subs, wchar_t *var, Term *value) {
	if(subs->nb_vars == subs->max_vars)
		return 0;
	subs->domain[subs->nb_vars] = malloc(sizeof(wchar_t)*(wcslen(var)+1));
	subs->range[subs->nb_vars] = value;
	wcscpy(subs->domain[subs->nb_vars], var);
	hashmap_append(subs->indices, var, subs->nb_vars);
	subs->nb_vars++;
	term_increase_references(value);
	return 1;
}

/**
  * 
  * This function gets the term linked with a variable
  * from a substitution. If variable is not contained 
  * in the substitution, returns NULL.
  * 
  **/
Term *substitution_get_link(Substitution *subs, wchar_t *var) {
	int index = hashmap_lookup(subs->indices, var);
	if(index != -1)
		return subs->range[index];
	return NULL;
}

/**
  *
  * This function composes two substitutions. This
  * function modifies the original first substitution.
  * 
  **/
Substitution *substitution_compose(Substitution *u, Substitution *v, int join) {
	int i;
	Term *apply;
	Substitution *subs = substitution_alloc(join ? u->nb_vars+v->nb_vars : u->nb_vars);
	for(i = 0; i < u->nb_vars; i++) {
		apply = term_apply_substitution(u->range[i], v);
		substitution_add_link(subs, u->domain[i], apply);
		term_free(apply);
	}
	if(join)
		for(i = 0; i < v->nb_vars; i++)
			substitution_add_link(subs, v->domain[i], v->range[i]);
	return subs;
}

/**
  *
  * This function applies a substitution to a term.
  * 
  **/
Term *term_apply_substitution(Term *term, Substitution *subs) {
	Term *term2, *tail;
	if(term->type == TYPE_VARIABLE) {
		term2 = substitution_get_link(subs, term->term.string);
		if(term2 == NULL) {
			term_increase_references(term);
			return term;
		} else {
			term_increase_references(term2);
			return term2;
		}
	} else if(term->type == TYPE_LIST) {
		if(term_list_is_null(term)) {
			term_increase_references(term);
			return term;
		}
		term2 = term_list_create(
			term_apply_substitution(term->term.list->head, subs),
			term_apply_substitution(term->term.list->tail, subs));
		term2->parent = term->parent;
		return term2;
	} else {
		term_increase_references(term);
		return term;
	}
}

/**
  * 
  * This function prints for the standard output a substitution.
  * 
  **/
void substitution_print(Substitution *subs) {
	int i;
	if(subs == NULL) {
		printf("false");
		return;
	} else if(subs->nb_vars == 0) {
		printf("true");
		return;
	} else if(subs->nb_vars == 1 && wcscmp(subs->domain[0], L"$error") == 0) {
		printf("\x1b[1m\x1b[31muncaught exception:\x1b[0m ");
		term_print(subs->range[0]);
		return;
	}
	printf("{");
	for(i = 0; i < subs->nb_vars; i++) {
		if(i > 0) printf(", ");
		printf("\x1b[1m\x1b[36m%ls\x1b[0m/", subs->domain[i]);
		term_print(subs->range[i]);
	}
	printf("}");
}