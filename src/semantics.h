/*H*
 * 
 * FILENAME: semantics.h
 * DESCRIPTION: Declarative semantics for the language
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 03.04.2019
 * 
 *H*/

#include "term.h"
#include "substitution.h"
#include "program.h"
#include "derivation.h"
#include "unification.h"
#include "builtin.h"
#include "exception.h"



/**
  * 
  * This function creates a derivation from a new goal,
  * returning a pointer to a newly initialized Derivation
  * struct.
  *
  **/
Derivation *semantics_query(Term *goal);

/**
  * 
  * This function finds and returns the next computed
  * answer of a derivation.
  *
  **/
Substitution *semantics_answer(Program *program, Derivation *D);