/*H*
 * 
 * FILENAME: program.c
 * DESCRIPTION: Data structures and functions for storing and manipuling programs
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 16.11.2019
 * 
 *H*/

#include "program.h"



/**
  * 
  * This function creates a program returning a pointer
  * to a newly initialized Program struct.
  * 
  **/
Program *program_alloc() {
	Module *module;
	Program *program = malloc(sizeof(Program));
	program->modules = malloc(sizeof(Module*)*N_MODULES);
	program->indices = hashmap_alloc(N_MODULES);
	program->nb_modules = 0;
	program->max_modules = N_MODULES;
	program->renames = 0;
	// Add user-defined module
	module = module_alloc();
	module_set_name(module, L"user");
	program_add_module(program, module);
	return program;
}

/**
  * 
  * This function increases the memory reserved for
  * modules in a program. Returns 0 if the request fails,
  * or 1 if it succeeds.
  * 
  **/
int program_realloc(Program *program) {
	program->max_modules += N_MODULES;
	program->modules = realloc(program->modules, sizeof(Module*)*program->max_modules);
	return program->modules != NULL;
}

/**
  * 
  * This function frees a previously allocated program.
  * The modules, predicates, clauses and terms underlying
  * the program will also be deallocated.
  * 
  **/
void program_free(Program *program) {
	int i;
	for(i = 0; i < program->nb_modules; i++)
		module_free(program->modules[i]);
	hashmap_free(program->indices);
	free(program->modules);
	free(program);
}

/**
  * This function checks if a program cannot store new
  * modules.
  * 
  **/
int program_is_full(Program *program) {
	return program->nb_modules == program->max_modules;
}

/**
  * 
  * This function adds a new module to a program. If the
  * memory of the program is full, the function resizes
  * it. Returns 0 if the request fails, or 1 if it succeeds.
  * 
  **/
int program_add_module(Program *program, Module *module) {
  int key;
	if(program_is_full(program))
		if(program_realloc(program) == 0)
			return 0;
  key = hashmap_lookup(program->indices, module->name);
  if(key != -1) {
    module_free(program->modules[key]);
    program->modules[key] = module;
  } else {
    hashmap_append(program->indices, module->name, program->nb_modules);
    program->modules[program->nb_modules] = module;
    program->nb_modules++;
  }
	return 1;
}

/**
  * 
  * This function returns the module of the program
  * by its identifier. If the module does not exist
  * in the program, returns NULL.
  * 
  **/
Module *program_get_module(Program *program, wchar_t *module_name) {
	int index = hashmap_lookup(program->indices, module_name);
	if(index == -1)
		return NULL;
	return program->modules[index];
}

/**
  * 
  * This function returns the predicate of the program
  * by its identifier. If the predicate does not exist
  * in the program, returns NULL.
  * 
  **/
Rule *program_get_predicate(Program *program, wchar_t *predicate_name, wchar_t *from) {
	int i;
	Rule *rule = NULL;
	for(i = 0; i < program->nb_modules && rule == NULL; i++)
		rule = module_get_predicate(program->modules[i], predicate_name, from);
	return rule;
}

/**
  * 
  * This function prints for the standard output
  * the list of predicates stored in a program, with
  * the format "name/arity :: type".
  * 
  **/
void program_listing(Program *program) {
	int i;
	for(i = 0; i < program->nb_modules; i++) {
		printf("(module %ls)\n", program->modules[i]->name);
		module_listing(program->modules[i]);
	}
}

/**
  * 
  * This function prints for the standard output
  * the whole program.
  * 
  **/
void program_print(Program *program) {
	int i;
	for(i = 0; i < program->nb_modules; i++) {
		module_print(program->modules[i]);
	}
}