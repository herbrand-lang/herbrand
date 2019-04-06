/*H*
 * 
 * FILENAME: module.c
 * DESCRIPTION: Data structures and functions for storing and manipuling modules
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 06.04.2019
 * 
 *H*/

#include "module.h"



/**
  * 
  * This function creates a module returning a pointer
  * to a newly initialized Module struct.
  * 
  **/
Module *module_alloc() {
	Module *module = malloc(sizeof(Module));
	module->predicates = malloc(sizeof(Rule*)*N_PREDICATES);
    module->name = NULL;
	module->indices = hashmap_alloc(N_PREDICATES);
	module->nb_predicates = 0;
	module->max_predicates = N_PREDICATES;
	return module;
}

/**
  * 
  * This function increases the memory reserved for
  * predicates in a module. Returns 0 if the request fails,
  * or 1 if it succeeds.
  * 
  **/
int module_realloc(Module *module) {
	module->max_predicates += N_PREDICATES;
	module->predicates = realloc(module->predicates, sizeof(Rule*)*module->max_predicates);
	return module->predicates != NULL;
}

/**
  * 
  * This function frees a previously allocated module.
  * The predicates, clauses and terms underlying the module
  * will also be deallocated.
  * 
  **/
void module_free(Module *module) {
	int i;
	for(i = 0; i < module->nb_predicates; i++)
		rule_free(module->predicates[i]);
	hashmap_free(module->indices);
    free(module->name);
	free(module->predicates);
	free(module);
}

/**
  *
  * This function sets the name of a module.
  * 
  **/
void module_set_name(Module *module, wchar_t *name) {
	if(module->name != NULL)
        free(module->name);
    module->name = malloc(sizeof(wchar_t)*(wcslen(name)+1));
    wcscpy(module->name, name);
}

/**
  *
  * This function checks if a module cannot store new
  * predicates.
  * 
  **/
int module_is_full(Module *module) {
	return module->nb_predicates == module->max_predicates;
}

/**
  * 
  * This function adds a new predicates to a module. If the
  * memory of the module is full, the function resizes
  * it. Returns 0 if the request fails, or 1 if it succeeds.
  * 
  **/
int module_add_predicate(Module *module, Rule *rule) {
	if(module_is_full(module))
		if(module_realloc(module) == 0)
			return 0;
	hashmap_append(module->indices, rule->name, module->nb_predicates);
	module->predicates[module->nb_predicates] = rule;
	module->nb_predicates++;
	return 1;
}

/**
  * 
  * This function returns the predicate of the module
  * by its identifier. If the predicate does not exist
  * in the module, returns NULL.
  * 
  **/
Rule *module_get_predicate(Module *module, wchar_t *predicate_name, wchar_t *from) {
	int index_from, index_pred = hashmap_lookup(module->indices, predicate_name);
	if(index_pred == -1)
		return NULL;
    if(module->predicates[index_pred]->local) {
        if(from == NULL)
            return NULL;
        index_from = hashmap_lookup(module->indices, from);
        if(index_from == -1)
            return NULL;
    }
	return module->predicates[index_pred];
}

/**
  * 
  * This function prints for the standard output
  * the list of predicates stored in a module, with
  * the format "name/arity :: type".
  * 
  **/
void module_listing(Module *module) {
	int i;
	for(i = 0; i < module->nb_predicates; i++) {
		printf("%ls/%d :: ", module->predicates[i]->name, module->predicates[i]->arity);
		term_print(module->predicates[i]->type);
		printf(module->predicates[i]->dynamic ? " #dynamic" : " #static");
		printf(module->predicates[i]->determinist ? " #det" : " #nondet");
		printf("\n");
	}
}

/**
  * 
  * This function prints for the standard output
  * the whole module.
  * 
  **/
void module_print(Module *module) {
	int i;
	for(i = 0; i < module->nb_predicates; i++) {
		rule_print(module->predicates[i]);
	}
}