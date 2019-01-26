/*H*
 * 
 * FILENAME: tokenizer.h
 * DESCRIPTION: Split source code into lexical components
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 25.01.2019
 * 
 *H*/

#include <stdio.h>
#include <stdlib.h>



#ifndef LOGIC_TOKENIZER_H
#define LOGIC_TOKENIZER_H
#define N_TOKENS 100
#define N_CHARS_TOKEN 16

typedef enum {TOKEN_WHITESPACE, TOKEN_LPAR, TOKEN_RPAR, TOKEN_BAR, TOKEN_ATOM, TOKEN_NUMBER, TOKEN_STRING, TOKEN_VARIABLE, TOKEN_ERROR} Category;

typedef struct Token {
    char *text;
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

#endif



/**
  * 
  * This function ...
  * 
  **/
Tokenizer *tokenizer_alloc();
/**
  * 
  * This function ...
  * 
  **/
int tokenizer_realloc(Tokenizer *tokenizer);
/**
  * 
  * This function ...
  * 
  **/
void tokenizer_free(Tokenizer *tokenizer);
/**
  * 
  * This function ...
  * 
  **/
int tokenizer_is_full(Tokenizer *tokenizer);
/**
  * 
  * This function ...
  * 
  **/
void *tokenizer_init_token(Tokenizer *tokenizer, int token);
/**
  * 
  * This function ...
  * 
  **/
int tokenizer_add_char_token(Tokenizer *tokenizer, int token, char character);
/**
  * 
  * This function ...
  * 
  **/
Tokenizer *tokenizer_read_stream(FILE *stream);
/**
  * 
  * This function ...
  * 
  **/
void tokenizer_read_string(Tokenizer *tokenizer, int token, FILE *stream);
/**
  * 
  * This function ...
  * 
  **/
void tokenizer_read_variable(Tokenizer *tokenizer, int token, FILE *stream);
/**
  * 
  * This function ...
  * 
  **/
void tokenizer_read_number(Tokenizer *tokenizer, int token, FILE *stream);
/**
  * 
  * This function ...
  * 
  **/
void tokenizer_read_atom(Tokenizer *tokenizer, int token, FILE *stream);