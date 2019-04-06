/*H*
 * 
 * FILENAME: main.h
 * DESCRIPTION: Main file
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 06.04.2019
 * 
 *H*/

#include <stdio.h>
#include <locale.h>
#include <wchar.h>
#include "program.h"
#include "parser.h"
#include "semantics.h"



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