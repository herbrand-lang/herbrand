/*H*
 * 
 * FILENAME: program.h
 * DESCRIPTION: Data structures and functions for storing and manipuling programs
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 27.01.2019
 * 
 *H*/

#include <stdio.h>
#include <stdlib.h>
#include "rule.h"
#include "hashmap.h"
#define N_RULES 32



#ifndef LOGIC_PROGRAM_H
#define LOGIC_PROGRAM_H

typedef struct Program {
	Rule **rules;
	Hashmap **indices;
	int nb_rules;
	int max_rules;
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
  * rules in a program. Returns 0 if the request fails,
  * or 1 if it succeeds.
  * 
  **/
int program_realloc(Program *program);

/**
  * This function checks if a program cannot store new
  * rules.
  * 
  **/
int program_is_full(Program *program);

/**
  * 
  * This function adds a new rule to a program. If the
  * memory of the program is full, the function resizes
  * it. Returns 0 if the request fails, or 1 if it succeeds.
  * 
  **/
int program_add_rule(Program *program, Rule *rule);

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