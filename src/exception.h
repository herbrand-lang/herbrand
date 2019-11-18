/*H*
 * 
 * FILENAME: exception.c
 * DESCRIPTION: Functions for throwing Herbrand errors
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 18.11.2019
 * 
 *H*/

#ifndef HERBRAND_EXCEPTION_H
#define HERBRAND_EXCEPTION_H

#include "term.h"
#include "type_check.h"

/**
  * 
  * This function generates an instantiation error returning a
  * pointer to a newly initialized Term struct.
  * 
  **/
Term *exception_instantiation_error(wchar_t *level);

/**
  * 
  * This function generates a type error returning a pointer
  * to a newly initialized Term struct.
  * 
  **/
Term *exception_type_error(Term *type, Term *found, wchar_t *level);

/**
  * 
  * This function generates a domain error returning a pointer
  * to a newly initialized Term struct.
  * 
  **/
Term *exception_domain_error(wchar_t *domain, Term *found, wchar_t *level);

/**
  * 
  * This function generates an existence error returning a
  * pointer to a newly initialized Term struct.
  * 
  **/
Term *exception_existence_error(wchar_t *source, Term *found, wchar_t *level);

/**
  * 
  * This function generates an arity error returning a
  * pointer to a newly initialized Term struct.
  * 
  **/
Term *exception_arity_error(int arity, int given, Term *found, wchar_t *level);

#endif