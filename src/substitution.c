/*H*
 * 
 * FILENAME: program.c
 * DESCRIPTION: Data structures and functions for storing and manipuling substitutions
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 27.03.2019
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
	subs->domain = malloc(sizeof(char*)*nb_vars);
	subs->range = malloc(sizeof(Term*)*nb_vars);
	subs->indices = hashmap_alloc(nb_vars);
	subs->nb_vars = 0;
	subs->max_vars = nb_vars;
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
	for(i = 0; i < subs->nb_vars; i++) {
		free(subs->domain[i]);
		term_free(subs->range[i]);
	}
	hashmap_free(subs->indices);
	free(subs);
}

/**
  * 
  * This function adds a new link into a substitution.
  * Returns 0 if the request fails, or 1 if it succeeds.
  * 
  **/
int substitution_add_link(Substitution *subs, Term *var, Term *value) {
	if(subs->nb_vars == subs->max_vars)
		return 0;
	subs->domain[subs->nb_vars] = malloc(sizeof(char)*(strlen(var->term.string)+1));
	subs->range[subs->nb_vars] = value;
	strcpy(subs->domain[subs->nb_vars], var->term.string);
	hashmap_append(subs->indices, var->term.string, subs->nb_vars);
	subs->nb_vars++;
	return 1;
}

/**
  * 
  * This function gets the term linked with a variable
  * from a substitution. If variable is not contained 
  * in the substitution, returns NULL.
  * 
  **/
Term *substitution_get_link(Substitution *subs, Term *var) {
	int index = hashmap_lookup(subs->indices, var->term.string);
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
void substitution_compose(Substitution *u, Substitution *v) {
	int i;
	for(i = 0; i < u->nb_vars; i++) 
		u->range[i] = term_apply_substitution(u->range[i], v);
}

/**
  *
  * This function applies a substitution to a term.
  * 
  **/
Term *term_apply_substitution(Term *term, Substitution *subs) {
	Term *term2;
	if(term->type == TYPE_VARIABLE) {
		term2 = substitution_get_link(subs, term);
		if(term2 == NULL)
			return term;
		else
			return term2;
	} else if(term->type == TYPE_LIST) {
		if(term_list_is_null(term))
			return term;
		term2 = term_list_empty();
		while(term->type == TYPE_LIST && !term_list_is_null(term)) {
			term_list_add_element(term2, term_apply_substitution(term_list_get_head(term), subs));
			term = term_list_get_tail(term);
		}
		term_list_set_tail(term2, term_apply_substitution(term, subs));
	} else {
		return term;
	}
}