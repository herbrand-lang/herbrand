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

typedef struct Parser {
    void *value;
    int start;
    int next;
    int success;
    char error[100];
} Parser;

#endif



/**
  * 
  * This function frees a previously allocated state.
  * The terms underlying the rule will not be deallocated.
  * 
  **/
void parser_free(Parser *state);

/**
  * 
  * This function parses a term from a stream.
  * 
  **/
Term *parser_term(FILE *stream);

/**
  * 
  * This function parses an stream and loads a program.
  * 
  **/
void parser_stream(Program *program, FILE *stream);

/**
  * 
  * This function takes a tokenizer and loads a program.
  * 
  **/
void parser_program(Program *program, Tokenizer *tokenizer);

/**
  * 
  * This function parses a predicate declaration.
  * 
  **/
Parser *parser_predicate(Tokenizer *tokenizer, int start);

/**
  * 
  * This function parses a clause of a predicate declaration.
  * 
  **/
Parser *parser_clause(Tokenizer *tokenizer, int start, int arity);

/**
  * 
  * This function parses an expression.
  * 
  **/
Parser *parser_expression(Tokenizer *tokenizer, int start);