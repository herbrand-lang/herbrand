/*H*
 * 
 * FILENAME: program.h
 * DESCRIPTION: Data structures and functions for storing and manipuling programs
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 06.04.2019
 * 
 *H*/

#include <stdio.h>
#include <stdlib.h>
#include "module.h"
#include "hashmap.h"
#define N_MODULES 8



#ifndef LOGIC_PROGRAM_H
#define LOGIC_PROGRAM_H

typedef struct Program {
	Module **modules;
	Hashmap *indices;
	int nb_modules;
	int max_modules;
  int renames;
} Program;

#endif



/**
  * 
  * This function creates a program returning a pointer
  * to a newly initialized Program struct.
  * 
  **/
Program *program_alloc();

/**
  * 
  * This function increases the memory reserved for
  * modules in a program. Returns 0 if the request fails,
  * or 1 if it succeeds.
  * 
  **/
int program_realloc(Program *program);

/**
  * 
  * This function frees a previously allocated program.
  * The modules, predicates, clauses and terms underlying
  * the program will also be deallocated.
  * 
  **/
void program_free(Program *program);

/**
  * This function checks if a program cannot store new
  * modules.
  * 
  **/
int program_is_full(Program *program);

/**
  * 
  * This function adds a new module to a program. If the
  * memory of the program is full, the function resizes
  * it. Returns 0 if the request fails, or 1 if it succeeds.
  * 
  **/
int program_add_module(Program *program, Module *module);

/**
  * 
  * This function returns the module of the program
  * by its identifier. If the module does not exist
  * in the program, returns NULL.
  * 
  **/
Module *program_get_module(Program *program, wchar_t *module_name);

/**
  * 
  * This function returns the predicate of the program
  * by its identifier. If the predicate does not exist
  * in the program, returns NULL.
  * 
  **/
Rule *program_get_predicate(Program *program, wchar_t *predicate_name, wchar_t *from);

/**
  * 
  * This function prints for the standard output
  * the list of predicates stored in a program, with
  * the format "name/arity :: type".
  * 
  **/
void program_listing(Program *program);

/**
  * 
  * This function prints for the standard output
  * the whole program.
  * 
  **/
void program_print(Program *program);