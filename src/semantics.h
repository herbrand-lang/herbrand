/*H*
 * 
 * FILENAME: semantics.h
 * DESCRIPTION: Declarative semantics for the language
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 01.02.2019
 * 
 *H*/

#include <string.h>
#include "term.h"
#include "substitution.h"



/**
  * 
  * This function returns the most general unifier of two terms.
  * If terms are not unifiable, returns NULL.
  * 
  **/
Substitution *semantics_unify_terms(Term *term1, Term *term2, int occurs_check);

/**
  * 
  * This function returns the most general unifier of two lists.
  * If lists are not unifiable, returns NULL.
  * 
  **/
Substitution *semantics_unify_lists(Term *term1, Term *term2, int occurs_check);

/**
  * 
  * This function returns the most general unifier of two numerals.
  * If numerals are not unifiable, returns NULL.
  * 
  **/
Substitution *semantics_unify_numerals(Term *num1, Term *num2);

/**
  * 
  * This function returns the most general unifier of two decimals.
  * If decimals are not unifiable, returns NULL.
  * 
  **/
Substitution *semantics_unify_decimals(Term *dec1, Term *dec2);

/**
  * 
  * This function returns the most general unifier of two atoms.
  * If atoms are not unifiable, returns NULL.
  * 
  **/
Substitution *semantics_unify_atoms(Term *atom1, Term *atom2);

/**
  * 
  * This function returns the most general unifier of two strings.
  * If strings are not unifiable, returns NULL.
  * 
  **/
Substitution *semantics_unify_strings(Term *str1, Term *str2);