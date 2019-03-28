/*H*
 * 
 * FILENAME: main.h
 * DESCRIPTION: Main file
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 27.03.2019
 * 
 *H*/

#include "program.h"
#include "parser.h"
#include "semantics.h"
#include <stdio.h>



/**
  * 
  * This function initilizes the interpreter.
  * 
  **/
int main(int argc, char *argv[]);

/**
  * 
  * This function starts the interactive
  * query mode.
  * 
  **/
void interactive_query();

/**
  * 
  * This function starts the interactive
  * unification mode.
  * 
  **/
void interactive_unification();