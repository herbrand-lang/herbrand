/*H*
 * 
 * FILENAME: tokenizer.h
 * DESCRIPTION: Split source code into lexical components
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 18.11.2019
 * 
 *H*/

#ifndef HERBRAND_TOKENIZER_H
#define HERBRAND_TOKENIZER_H

#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>

#define N_TOKENS 100
#define N_CHARS_TOKEN 16

typedef enum {TOKEN_WHITESPACE, TOKEN_LPAR, TOKEN_RPAR, TOKEN_BAR, TOKEN_ATOM, TOKEN_NUMERAL, TOKEN_DECIMAL, TOKEN_STRING, TOKEN_CHAR, TOKEN_VARIABLE, TOKEN_TAG, TOKEN_ERROR} Category;

typedef struct Token {
    wchar_t *text;
    int length;
    int max_length;
    int line;
    int column;
    Category category;
} Token;

typedef struct Tokenizer {
    Token **tokens;
    int nb_tokens;
    int max_tokens;
    int line;
    int column;
} Tokenizer;

/**
  * 
  * This function creates a tokenizer, returning a pointer
  * to a newly initialized Tokenizer struct.
  * 
  **/
Tokenizer *tokenizer_alloc();

/**
  * 
  * This function increases the memory reserved for
  * tokens in a tokenizer. Returns 0 if the request fails,
  * or 1 if it succeeds.
  * 
  **/
int tokenizer_realloc(Tokenizer *tokenizer);

/**
  * 
  * This function frees a previously allocated tokenizer.
  * The tokens underlying the program will also be deallocated.
  * 
  **/
void tokenizer_free(Tokenizer *tokenizer);

/**
  *
  * This function checks if a tokenizer cannot store new
  * tokens.
  * 
  **/
int tokenizer_is_full(Tokenizer *tokenizer);

/**
  * 
  * This function allocates and initilizes the n-th
  * token in a tokenizer.
  * 
  **/
int tokenizer_init_token(Tokenizer *tokenizer);

/**
  * 
  * This function adds a character into a previously allocated token.
  * 
  **/
int tokenizer_add_char_token(Tokenizer *tokenizer, int token, wchar_t character);

/**
  * 
  * This function reads a stream and splits it into lexical components.
  * Allocates and returns a Tokenizer struct containing the resulting tokens.
  * 
  **/
Tokenizer *tokenizer_read_stream(FILE *stream);

#endif