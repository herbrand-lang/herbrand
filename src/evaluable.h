/*H*
 * 
 * FILENAME: evaluable.h
 * DESCRIPTION: Functions for evaluating arithmetic terms
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 18.11.2019
 * 
 *H*/

#ifndef HERBRAND_EVALUABLE_H
#define HERBRAND_EVALUABLE_H

#include <math.h>
#include "term.h"
#include "hashmap.h"
#include "exception.h"

#define EVALUABLE_PI 3.14159265359
#define EVALUABLE_TAU 6.28318530718
#define EVALUABLE_E 2.71828182846
#define EVALUABLE_HASH_SIZE 32
wchar_t *evaluable_keys[EVALUABLE_HASH_SIZE];
Term *(*evaluable_handlers[EVALUABLE_HASH_SIZE])();
int evaluable_arities[EVALUABLE_HASH_SIZE];

/**
  * 
  * This function cheks if an atom is a evaluable
  * term.
  * 
  **/
int evaluable_check_term(Term *term);

/**
  * 
  * This function evaluates an evaluable term.
  * 
  **/
Term *evaluable_eval_term(Term *term);

/**
  * 
  * This function evaluates an compares two evaluable terms.
  * 
  **/
int evaluable_compare_terms(Term *term1, Term* term2, Term **error);

// Evaluable terms
Term *evaluable_pi(Term *term);
Term *evaluable_e(Term *term);
Term *evaluable_tau(Term *term);
Term *evaluable_add(Term *term);
Term *evaluable_sub(Term *term);
Term *evaluable_mul(Term *term);
Term *evaluable_div(Term *term);

#endif