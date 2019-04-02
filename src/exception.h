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
  * This function generates a type error returning a pointer
  * to a newly initialized Term struct.
  * 
  **/
Term *exception_type_error(char *type, Term *found, char *level);