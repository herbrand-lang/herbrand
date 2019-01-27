/*H*
 * 
 * FILENAME: parse.h
 * DESCRIPTION: Parse programs
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 25.01.2019
 * 
 *H*/

#include <string.h>
#include "tokenizer.h"
#include "program.h"
#include "rule.h"
#include "clause.h"
#include "term.h"



#ifndef LOGIC_PARSER_H
#define LOGIC_PARSER_H

typedef struct ParserState {
    Term *value;
    int start;
    int success;
    char error[100];
} ParserState;

#endif



/**
  * 
  * This function ...
  * 
  **/
void parser_state_free(ParserState *state);
/**
  * 
  * This function ...
  * 
  **/
Program *parser_stream(FILE *stream);
/**
  * 
  * This function ...
  * 
  **/
Program *parser_program(Tokenizer *tokenizer);
/**
  * 
  * This function ...
  * 
  **/
ParserState *parser_expression(Tokenizer *tokenizer, int token);
/**
  * 
  * This function ...
  * 
  **/
int parser_declare_predicate(Program *program, Term *term);