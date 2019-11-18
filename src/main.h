/*H*
 * 
 * FILENAME: main.h
 * DESCRIPTION: Main file
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 18.11.2019
 * 
 *H*/

#ifndef HERBRAND_MAIN_H
#define HERBRAND_MAIN_H

#ifndef HERBRAND_PATH
#define HERBRAND_PATH L"/usr/local/herbrand/"
#endif

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

/**
  * 
  * This function creates a program with all information
  * about builtin predicates, returning a pointer to a
  * newly initialized Program struct.
  * 
  **/
Program *program_init();

#endif