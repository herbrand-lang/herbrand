/*H*
 * 
 * FILENAME: builtin.h
 * DESCRIPTION: Functions for evaluating built-in predicates
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 30.03.2019
 * 
 *H*/

#include "term.h"
#include "derivation.h"
#include "substitution.h"
#include "unification.h"
#include "program.h"



#ifndef LOGIC_BUILTIN_H
#define LOGIC_BUILTIN_H

void *builtin_array[1];

#endif



/**
  * 
  * This functions cheks if an atom is a built-in
  * predicate.
  * 
  **/
int builtin_check_predicate(Term *term);

/**
  * 
  * This functions runs a built-in predicate.
  * 
  **/
int builtin_run_predicate(Program *program, Derivation *D, State *point, Term *term);

/**
  * 
  * Syntactic unification (=/2)
  * 
  **/
void builtin_unification_2(Program *program, Derivation *D, State *point, Term *term);