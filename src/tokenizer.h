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

typedef enum {TOKEN_WHITESPACE, TOKEN_LPAR, TOKEN_RPAR, TOKEN_BAR, TOKEN_ATOM, TOKEN_NUMBER, TOKEN_STRING, TOKEN_VARIABLE, TOKEN_TAG, TOKEN_ERROR} Category;

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
void *tokenizer_init_token(Tokenizer *tokenizer, int token);

/**
  * 
  * This function adds a character into a previously allocated token.
  * 
  **/
int tokenizer_add_char_token(Tokenizer *tokenizer, int token, char character);

/**
  * 
  * This function reads a stream and splits it into lexical components.
  * Allocates and returns a Tokenizer struct containing the resulting tokens.
  * 
  **/
Tokenizer *tokenizer_read_stream(FILE *stream);

/**
  * 
  * This function reads a lexical component of the category string. A string
  * is any sequence between double quotes ('"').
  *
  **/
void tokenizer_read_string(Tokenizer *tokenizer, int token, FILE *stream);

/**
  * 
  * This function reads a lexical component of the category variable. A variable
  * is a non-empty sequence of letters (a-zA-Z) and symbols (-_) starting by
  * a uppercase letter (a-z).
  *
  **/
void tokenizer_read_variable(Tokenizer *tokenizer, int token, FILE *stream);

/**
  * 
  * This function reads a lexical component of the category number. A number
  * is a non-empty sequence of numbers (0-9).
  *
  **/
void tokenizer_read_number(Tokenizer *tokenizer, int token, FILE *stream);

/**
  * 
  * This function reads a lexical component of the category atom. An atom is
  * a non-empty sequence of letters (a-zA-Z) and symbols (-_) starting by
  * a lowercase letter (a-z).
  *
  **/
void tokenizer_read_atom(Tokenizer *tokenizer, int token, FILE *stream);

/**
  * 
  * This function reads a lexical component of the category tag. A tag is
  * the character '#' followed by a non-empty sequence of lowercase letters
  * (a-z).
  *
  **/
void tokenizer_read_tag(Tokenizer *tokenizer, int token, FILE *stream);