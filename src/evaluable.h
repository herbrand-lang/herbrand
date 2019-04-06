/*H*
 * 
 * FILENAME: evaluable.h
 * DESCRIPTION: Functions for evaluating arithmetic terms
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 07.04.2019
 * 
 *H*/

#include <math.h>
#include "term.h"
#include "hashmap.h"
#include "exception.h"



#ifndef LOGIC_EVALUABLE_H
#define LOGIC_EVALUABLE_H

#define EVALUABLE_PI 3.14159265359
#define EVALUABLE_TAU 6.28318530718
#define EVALUABLE_E 2.71828182846
#define EVALUABLE_HASH_SIZE 32
wchar_t *evaluable_keys[EVALUABLE_HASH_SIZE];
Term *(*evaluable_handlers[EVALUABLE_HASH_SIZE])();
int evaluable_arities[EVALUABLE_HASH_SIZE];

#endif



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

Term *evaluable_pi(Term *term);
Term *evaluable_e(Term *term);
Term *evaluable_tau(Term *term);
Term *evaluable_add(Term *term);
Term *evaluable_sub(Term *term);
Term *evaluable_mul(Term *term);
Term *evaluable_div(Term *term);