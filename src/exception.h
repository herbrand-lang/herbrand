/*H*
 * 
 * FILENAME: exception.c
 * DESCRIPTION: Functions for throwing Herbrand errors
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 03.04.2019
 * 
 *H*/

#include "term.h"



/**
  * 
  * This function generates an instantiation error returning a
  * pointer to a newly initialized Term struct.
  * 
  **/
Term *exception_instantiation_error(char *level);

/**
  * 
  * This function generates a type error returning a pointer
  * to a newly initialized Term struct.
  * 
  **/
Term *exception_type_error(char *type, Term *found, char *level);

/**
  * 
  * This function generates an existence error returning a
  * pointer to a newly initialized Term struct.
  * 
  **/
Term *exception_existence_error(char *source, Term *found, char *level);