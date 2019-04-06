/*H*
 * 
 * FILENAME: module.h
 * DESCRIPTION: Data structures and functions for storing and manipuling modules
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 06.04.2019
 * 
 *H*/

#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>
#include "rule.h"
#include "hashmap.h"
#define N_PREDICATES 32



#ifndef LOGIC_MODULE_H
#define LOGIC_MODULE_H

typedef struct Module {
	Rule **predicates;
	Hashmap *indices;
	wchar_t *name;
	int nb_predicates;
	int max_predicates;
} Module;

#endif



/**
  * 
  * This function creates a module returning a pointer
  * to a newly initialized Module struct.
  * 
  **/
Module *module_alloc();

/**
  * 
  * This function increases the memory reserved for
  * predicates in a module. Returns 0 if the request fails,
  * or 1 if it succeeds.
  * 
  **/
int module_realloc(Module *module);

/**
  * 
  * This function frees a previously allocated module.
  * The rules, clauses and terms underlying the module
  * will also be deallocated.
  * 
  **/
void module_free(Module *module);

/**
  *
  * This function sets the name of a module.
  * 
  **/
void module_set_name(Module *module, wchar_t *name);

/**
  * This function checks if a module cannot store new
  * predicates.
  * 
  **/
int module_is_full(Module *module);

/**
  * 
  * This function adds a new predicate to a module. If the
  * memory of the module is full, the function resizes
  * it. Returns 0 if the request fails, or 1 if it succeeds.
  * 
  **/
int module_add_predicate(Module *module, Rule *rule);

/**
  * 
  * This function returns the predicate of the module
  * by its identifier. If the predicate does not exist
  * in the program, returns NULL.
  * 
  **/
Rule *module_get_predicate(Module *module, wchar_t *predicate_name, wchar_t *from);

/**
  * 
  * This function prints for the standard output
  * the list of predicates stored in a module, with
  * the format "name/arity :: type".
  * 
  **/
void module_listing(Module *module);

/**
  * 
  * This function prints for the standard output
  * the whole program.
  * 
  **/
void module_print(Module *module);