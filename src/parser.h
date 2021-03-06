/*H*
 * 
 * FILENAME: parse.h
 * DESCRIPTION: Parse programs
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 18.11.2019
 * 
 *H*/

#ifndef HERBRAND_PARSER_H
#define HERBRAND_PARSER_H

#include <string.h>
#include "tokenizer.h"
#include "module.h"
#include "program.h"
#include "rule.h"
#include "clause.h"
#include "term.h"

typedef struct Parser {
    void *value;
    int start;
    int next;
    int success;
    wchar_t error[100];
} Parser;

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
  * This function parses a module declaration.
  * 
  **/
Parser *parser_module(Tokenizer *tokenizer, int start);

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
Parser *parser_clause(Tokenizer *tokenizer, int start, wchar_t *rule_name, int arity);

/**
  * 
  * This function parses an expression.
  * 
  **/
Parser *parser_expression(Tokenizer *tokenizer, int start);

#endif