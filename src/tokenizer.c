/*H*
 * 
 * FILENAME: tokenizer.c
 * DESCRIPTION: Split source code into lexical components
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 18.11.2019
 * 
 *H*/

#include "tokenizer.h"



/**
  * 
  * This function creates a tokenizer, returning a pointer
  * to a newly initialized Tokenizer struct.
  * 
  **/
Tokenizer *tokenizer_alloc() {
	Tokenizer *tokenizer = malloc(sizeof(Tokenizer));
	tokenizer->tokens = malloc(sizeof(Token*)*N_TOKENS);
	tokenizer->nb_tokens = 0;
	tokenizer->max_tokens = N_TOKENS;
	tokenizer->line = 1;
	tokenizer->column = 1;
	return tokenizer;
}

/**
  * 
  * This function increases the memory reserved for
  * tokens in a tokenizer. Returns 0 if the request fails,
  * or 1 if it succeeds.
  * 
  **/
int tokenizer_realloc(Tokenizer *tokenizer) {
	tokenizer->max_tokens += N_TOKENS;
	tokenizer->tokens = realloc(tokenizer->tokens, sizeof(Token*)*tokenizer->max_tokens);
	return tokenizer->tokens != NULL;
}

/**
  * 
  * This function frees a previously allocated tokenizer.
  * The tokens underlying the program will also be deallocated.
  * 
  **/
void tokenizer_free(Tokenizer *tokenizer) {
	int i;
	for(i = 0; i < tokenizer->nb_tokens; i++) {
		free(tokenizer->tokens[i]->text);
		free(tokenizer->tokens[i]);
	}
	free(tokenizer->tokens);
	free(tokenizer);
}

/**
  *
  * This function checks if a tokenizer cannot store new
  * tokens.
  * 
  **/
int tokenizer_is_full(Tokenizer *tokenizer) {
	return tokenizer->nb_tokens == tokenizer->max_tokens;
}

/**
  * 
  * This function allocates and initilizes the n-th
  * token in a tokenizer.
  * 
  **/
int tokenizer_init_token(Tokenizer *tokenizer) {
	Token *ptr_token;
	int token = tokenizer->nb_tokens;
	if(tokenizer_is_full(tokenizer))
			if(!tokenizer_realloc(tokenizer))
				return -1;
	tokenizer->tokens[token] = malloc(sizeof(Token));
	ptr_token = tokenizer->tokens[token];
	ptr_token->text = malloc(sizeof(wchar_t)*N_CHARS_TOKEN);
	ptr_token->text[0] = L'\0';
	ptr_token->length = 0;
	ptr_token->line = tokenizer->line;
	ptr_token->column = tokenizer->column;
	ptr_token->max_length = N_CHARS_TOKEN;
	tokenizer->nb_tokens++;
	return token;
}

/**
  * 
  * This function adds a character into a previously allocated token.
  * 
  **/
int tokenizer_add_char_token(Tokenizer *tokenizer, int token, wchar_t character) {
	Token *ptr_token = tokenizer->tokens[token];
	if(ptr_token->length == 0) {
		ptr_token->line = tokenizer->line;
		ptr_token->column = tokenizer->column;
	}
	if(ptr_token->length+1 == ptr_token->max_length) {
		ptr_token->max_length += N_CHARS_TOKEN;
		ptr_token->text = realloc(ptr_token->text, sizeof(wchar_t)*ptr_token->max_length);
		if(ptr_token->text == NULL)
			return 0;
	}
	ptr_token->text[ptr_token->length] = character;
	ptr_token->length++;
	ptr_token->text[ptr_token->length] = L'\0';
	return 1;
}

/**
  * 
  * This function reads a stream and splits it into lexical components.
  * Allocates and returns a Tokenizer struct containing the resulting tokens.
  * 
  **/
Tokenizer *tokenizer_read_stream(FILE *stream) {
	wchar_t character;
	int token, ignore, token_number_dot = 0, token_graphic = 0, token_char_open = 0, token_string_open = 0, token_start = 1, token_cat = -1, nb_paren = 0;
	Tokenizer *tokenizer = tokenizer_alloc();
	while(fscanf(stream, "%lc", &character) == 1 && !feof(stream) && stream != stdin ||
	stream == stdin && (nb_paren > 0 || tokenizer->nb_tokens == 0 || tokenizer->nb_tokens > 0 && character != L'\n' || !token_start && token_cat == TOKEN_STRING)) {
		// Read string
		if((token_start || token_cat == TOKEN_ATOM && token_graphic) && character == L'"' || !token_start && token_cat == TOKEN_STRING) {
			if(token_cat != TOKEN_STRING || token_start) {
				token = tokenizer_init_token(tokenizer);
				tokenizer->tokens[token]->category = TOKEN_STRING;
			}
			if(character != L'"')
				tokenizer_add_char_token(tokenizer, token, character);
			tokenizer->column++;
			token_cat = TOKEN_STRING;
			if(token_string_open && character == L'"') {
				token_string_open = 0;
				token_start = 1;
			} else {
				token_string_open = 1;
				token_start = 0;
			}
		// Read char
		} else if((token_start || token_cat == TOKEN_ATOM && token_graphic) && character == L'\'' || !token_start && token_cat == TOKEN_CHAR) {
			if(token_cat != TOKEN_CHAR || token_start) {
				token = tokenizer_init_token(tokenizer);
				tokenizer->tokens[token]->category = TOKEN_CHAR;
			}
			if(character != L'\'')
				tokenizer_add_char_token(tokenizer, token, character);
			tokenizer->column++;
			token_cat = TOKEN_CHAR;
			if(token_char_open && character == L'\'') {
				token_char_open = 0;
				token_start = 1;
				if(tokenizer->tokens[token]->length > 1) {
					tokenizer->tokens[token]->category = TOKEN_ERROR;
					break;
				}
			} else {
				token_char_open = 1;
				token_start = 0;
			}
		// Read tag
		} else if(character == L'#' || !token_start && token_cat == TOKEN_TAG &&
		character >= 97 && character <= 122) {
			if(token_cat != TOKEN_TAG || token_start) {
				token = tokenizer_init_token(tokenizer);
				tokenizer->tokens[token]->category = TOKEN_TAG;
				token_start = 0;
			}
			if(character != L'#')
				tokenizer_add_char_token(tokenizer, token, character);
			tokenizer->column++;
			token_cat = TOKEN_TAG;
		// Read comment
		} else if(character == L';') {
			while(!feof(stream) && character != L'\n') {
				tokenizer->column++;
				ignore = fscanf(stream, "%lc", &character);
			}
			tokenizer->column = 1;
			tokenizer->line++;
			if(character == EOF)
				break;
		// Read variable
		} else if((token_start || token_cat == TOKEN_ATOM && token_graphic) && (character >= 65 && character <= 90 || character == '_') ||
		!token_start && token_cat == TOKEN_VARIABLE &&
		(character >= 65 && character <= 90 || character >= 97 && character <= 122 || character >= 48 && character <= 57 || character == '_')) {
			if(token_cat != TOKEN_VARIABLE || token_start) {
				token = tokenizer_init_token(tokenizer);
				tokenizer->tokens[token]->category = TOKEN_VARIABLE;
				token_start = 0;
			}
			tokenizer_add_char_token(tokenizer, token, character);
			tokenizer->column++;
			token_cat = TOKEN_VARIABLE;
		// Read atom
		} else if((token_graphic || token_start) && character >= 97 && character <= 122 ||
		!token_start && token_cat == TOKEN_ATOM && !token_graphic &&
		(character >= 65 && character <= 90 || character >= 97 && character <= 122 || character >= 48 && character <= 57 || character == '_')) {
			if(token_cat != TOKEN_ATOM || token_cat == TOKEN_ATOM && token_graphic || token_start) {
				token = tokenizer_init_token(tokenizer);
				tokenizer->tokens[token]->category = TOKEN_ATOM;
				token_start = 0;
				token_graphic = 0;
			}
			tokenizer_add_char_token(tokenizer, token, character);
			tokenizer->column++;
			token_cat = TOKEN_ATOM;
		// Read graphic atom
		} else if(wcsrchr(L"+-*/^&:<>=?~$@!", character) != NULL) {
			if(token_cat != TOKEN_ATOM || token_cat == TOKEN_ATOM && !token_graphic || token_start) {
				token = tokenizer_init_token(tokenizer);
				tokenizer->tokens[token]->category = TOKEN_ATOM;
				token_start = 0;
				token_graphic = 1;
			}
			tokenizer_add_char_token(tokenizer, token, character);
			tokenizer->column++;
			token_cat = TOKEN_ATOM;
		// Read number
		} else if(character >= 48 && character <= 57 || !token_start && !token_number_dot && character == L',') {
			if(token_start || token_cat != TOKEN_ATOM && (!token_graphic || tokenizer->tokens[token]->length != 1
			|| tokenizer->tokens[token]->text[0] != L'-') && (token_cat != TOKEN_NUMERAL || token_start)) {
				token = tokenizer_init_token(tokenizer);
				tokenizer->tokens[token]->category = TOKEN_NUMERAL;
				token_start = 0;
				token_number_dot = 0;
			}
			if(tokenizer->tokens[token]->category == TOKEN_ATOM)
				tokenizer->tokens[token]->category = TOKEN_NUMERAL;
			token_graphic = 0;
			if(character == L',') {
				token_number_dot = 1;
				tokenizer->tokens[token]->category = TOKEN_DECIMAL;
			}
			tokenizer_add_char_token(tokenizer, token, character);
			tokenizer->column++;
			token_cat = TOKEN_NUMERAL;
		// Read left parenthesis
		} else if(character == L'(') {
			token = tokenizer_init_token(tokenizer);
			tokenizer->tokens[token]->category = TOKEN_LPAR;
			tokenizer_add_char_token(tokenizer, token, character);
			tokenizer->column++;
			token_start = 1;
			token_cat = TOKEN_LPAR;
			nb_paren++;
		// Read right parenthesis
		} else if(character == L')') {
			token = tokenizer_init_token(tokenizer);
			tokenizer->tokens[token]->category = TOKEN_RPAR;
			tokenizer_add_char_token(tokenizer, token, character);
			tokenizer->column++;
			token_start = 1;
			token_cat = TOKEN_RPAR;
			nb_paren--;
		// Read bar
		} else if(character == L'|') {
			token = tokenizer_init_token(tokenizer);
			tokenizer->tokens[token]->category = TOKEN_BAR;
			tokenizer_add_char_token(tokenizer, token, character);
			tokenizer->column++;
			token_start = 1;
			token_cat = TOKEN_BAR;
		// Read whitespace
		} else if(character == L' ' || character == L'\t' || character == L'\r') {
			tokenizer->column++;
			token_start = 1;
			token_cat = TOKEN_WHITESPACE;
		// Read break line
		} else if(character == L'\n') {
			tokenizer->line++;
			tokenizer->column = 1;
			token_start = 1;
			token_cat = TOKEN_WHITESPACE;
		// Read unexpected input
		} else {
			token = tokenizer_init_token(tokenizer);
			tokenizer->tokens[token]->category = TOKEN_ERROR;
			tokenizer_add_char_token(tokenizer, token, character);
			tokenizer->column++;
			token_start = 1;
			token_cat = TOKEN_ERROR;
			break;
		}
	}
	if(tokenizer->nb_tokens > 0 && tokenizer->tokens[token]->category == TOKEN_STRING && !token_start)
		tokenizer->tokens[token]->category = TOKEN_ERROR;
	return tokenizer;
}