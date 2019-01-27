/*H*
 * 
 * FILENAME: structures.h
 * DESCRIPTION: Data structures
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 25.01.2019
 * 
 *H*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hashmap.h"
#define N_RULES 32



#ifndef LOGIC_STRUCTURES_H
#define LOGIC_STRUCTURES_H

typedef enum {ATOM, VARIABLE, NUMERAL, DECIMAL, STRING, LIST} Type;

typedef struct Term {
	union {
		int numeral;
		double decimal;
		char *string;
		struct List *list;
	} term;
	Type type;
} Term;

typedef struct List {
	struct Term *head;
	struct Term *tail;
} List;

typedef struct Clause {
	Term **head;
	Term **body;
	int nb_args;
	int nb_atoms;
} Clause;

typedef struct Rule {
	Clause **clauses;
	Term **type;
	char *name;
	int arity;
	int nb_clauses;
} Rule;

typedef struct Program {
	Rule **rules;
	Hashmap **indices;
	int nb_rules;
	int max_rules;
} Program;

#endif



/**
  * 
  * This function ...
  * 
  **/
Program *program_alloc();
/**
  * 
  * This function ...
  * 
  **/
int program_realloc(Program *program);
/**
  * 
  * This function ...
  * 
  **/
int program_is_full(Program *program);
/**
  * 
  * This function ...
  * 
  **/
int program_add_rule(Program *program, char *name, int arity, int nb_clauses, Term **type, Clause **clauses);
/**
  * 
  * This function ...
  * 
  **/
Rule *rule_alloc(int name_length, int arity, int nb_clauses);
/**
  * 
  * This function creates a clause with $nb_atoms atoms in the body,
  * returning a pointer to a newly initialized Clause struct.
  * 
  **/
Clause *clause_alloc(int nb_args, int nb_atoms);
/**
  * 
  * This function ...
  * 
  **/
void program_listing(Program *program);
/**
  * 
  * This function ...
  * 
  **/
void term_print(Term *term);