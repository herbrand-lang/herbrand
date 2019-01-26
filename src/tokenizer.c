/*H*
 * 
 * FILENAME: tokenizer.c
 * DESCRIPTION: Split source code into lexical components
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 25.01.2019
 * 
 *H*/

#include "tokenizer.h"



/**
  * 
  * This function ...
  * 
  **/
Tokenizer *tokenizer_alloc() {
	Tokenizer *tokenizer = malloc(sizeof(Tokenizer));
	tokenizer->tokens = malloc(sizeof(Token*)*N_TOKENS);
	tokenizer->nb_tokens = 0;
	tokenizer->max_tokens = N_TOKENS;
	tokenizer->line = 0;
	tokenizer->column = 0;
	return tokenizer;
}

/**
  * 
  * This function ...
  * 
  **/
int tokenizer_realloc(Tokenizer *tokenizer) {
	tokenizer->max_tokens += N_TOKENS;
	tokenizer->tokens = realloc(tokenizer->tokens, sizeof(Token*)*tokenizer->max_tokens);
	return tokenizer->tokens != NULL;
}

/**
  * 
  * This function ...
  * 
  **/
void tokenizer_free(Tokenizer *tokenizer) {
	int i;
	for(i = 0; i <= tokenizer->nb_tokens; i++) {
		free(tokenizer->tokens[i]->text);
		free(tokenizer->tokens[i]);
	}
	free(tokenizer->tokens);
	free(tokenizer);
}

/**
  * 
  * This function ...
  * 
  **/
int tokenizer_is_full(Tokenizer *tokenizer) {
	return tokenizer->nb_tokens == tokenizer->max_tokens;
}

/**
  * 
  * This function ...
  * 
  **/
void *tokenizer_init_token(Tokenizer *tokenizer, int token) {
	Token *ptr_token;
	tokenizer->tokens[token] = malloc(sizeof(Token));
	ptr_token = tokenizer->tokens[token];
	ptr_token->text = malloc(sizeof(char)*N_CHARS_TOKEN);
	ptr_token->text[0] = '\0';
	ptr_token->length = 0;
	ptr_token->line = tokenizer->line;
	ptr_token->column = tokenizer->column;
	ptr_token->max_length = N_CHARS_TOKEN;
}

/**
  * 
  * This function ...
  * 
  **/
int tokenizer_add_char_token(Tokenizer *tokenizer, int token, char character) {
	Token *ptr_token = tokenizer->tokens[token];
	if(ptr_token->length+1 == ptr_token->max_length) {
		ptr_token->max_length += N_CHARS_TOKEN;
		ptr_token->text = realloc(ptr_token->text, sizeof(char)*ptr_token->max_length);
		if(ptr_token->text == NULL)
			return 0;
	}
	ptr_token->text[ptr_token->length] = character;
	ptr_token->length++;
	ptr_token->text[ptr_token->length] = '\0';
	return 1;
}

/**
  * 
  * This function ...
  * 
  **/
Tokenizer *tokenizer_read_stream(FILE *stream) {
	char character;
	int token = 0;
	Tokenizer *tokenizer = tokenizer_alloc();
	tokenizer_init_token(tokenizer, 0);
	while(fscanf(stream, "%c", &character) != EOF) {
		tokenizer->column++;
		// Read string
		if(character == '"') {
			tokenizer->tokens[token]->category = TOKEN_STRING;
			tokenizer_add_char_token(tokenizer, token, character);
			tokenizer_read_string(tokenizer, token, stream);
		// Read variable
		} else if(character >= 65 && character <= 90) {
			tokenizer->tokens[token]->category = TOKEN_VARIABLE;
			tokenizer_add_char_token(tokenizer, token, character);
			tokenizer_read_variable(tokenizer, token, stream);
		// Read atom
		} else if(character >= 97 && character <= 122) {
			tokenizer->tokens[token]->category = TOKEN_ATOM;
			tokenizer_add_char_token(tokenizer, token, character);
			tokenizer_read_atom(tokenizer, token, stream);
		// Read number
		} else if(character >= 48 && character <= 57) {
			tokenizer->tokens[token]->category = TOKEN_NUMBER;
			tokenizer_add_char_token(tokenizer, token, character);
			tokenizer_read_number(tokenizer, token, stream);
		// Read left parenthesis
		} else if(character == '(') {
			tokenizer->tokens[token]->category = TOKEN_LPAR;
			tokenizer_add_char_token(tokenizer, token, character);
		// Read right parenthesis
		} else if(character == ')') {
			tokenizer->tokens[token]->category = TOKEN_RPAR;
			tokenizer_add_char_token(tokenizer, token, character);
		// Read bar
		} else if(character == '|') {
			tokenizer->tokens[token]->category = TOKEN_BAR;
			tokenizer_add_char_token(tokenizer, token, character);
		// Read whitespace
		} else if(character == ' ' || character == '\t') {
			continue;
		// Read break line
		} else if(character == '\n') {
			tokenizer->line++;
			tokenizer->column = 0;
			continue;
		// Read unexpected input
		} else {
			printf("unexpected input\n");
			return tokenizer;
		}
		// Resize text of the token
		tokenizer->tokens[token]->text = realloc(tokenizer->tokens[token]->text, sizeof(char)*(tokenizer->tokens[token]->length+1));
		// Reallocate tokens if needs
		if(tokenizer_is_full(tokenizer))
			if(!tokenizer_realloc(tokenizer))
				return NULL;
		token++;
		tokenizer->nb_tokens++;
		tokenizer_init_token(tokenizer, token);
	}
	return tokenizer;
}

/**
  * 
  * This function ...
  * 
  **/
void tokenizer_read_string(Tokenizer *tokenizer, int token, FILE *stream) {
	char character;
	while(fscanf(stream, "%c", &character) != EOF) {
		tokenizer->column++;
		if(character == '\n') {
			tokenizer->line++;
			tokenizer->column = 0;
		} else if(character == '"') {
			fscanf(stream, "%c", &character);
			if(character != '"') {
				fseek(stream, -1, SEEK_CUR);
				return;
			}
			tokenizer->column++;
		}
		tokenizer_add_char_token(tokenizer, token, character);
	}
}

/**
  * 
  * This function ...
  * 
  **/
void tokenizer_read_variable(Tokenizer *tokenizer, int token, FILE *stream) {
	char character;
	while(fscanf(stream, "%c", &character) != EOF) {
		if(character >= 65 && character <= 90 || character >= 97 && character <= 122 || character >= 48 && character <= 57) {
			tokenizer_add_char_token(tokenizer, token, character);
			tokenizer->column++;
		} else {
			fseek(stream, -1, SEEK_CUR);
			return;
		}
	}
}

/**
  * 
  * This function ...
  * 
  **/
void tokenizer_read_number(Tokenizer *tokenizer, int token, FILE *stream) {
	char character;
	while(fscanf(stream, "%c", &character) != EOF) {
		if(character >= 48 && character <= 57) {
			tokenizer_add_char_token(tokenizer, token, character);
			tokenizer->column++;
		} else {
			fseek(stream, -1, SEEK_CUR);
			return;
		}
	}
}

/**
  * 
  * This function ...
  * 
  **/
void tokenizer_read_atom(Tokenizer *tokenizer, int token, FILE *stream) {
	char character;
	while(fscanf(stream, "%c", &character) != EOF) {
		if(character >= 65 && character <= 90 || character >= 97 && character <= 122 || character >= 48 && character <= 57) {
			tokenizer_add_char_token(tokenizer, token, character);
			tokenizer->column++;
		} else {
			fseek(stream, -1, SEEK_CUR);
			return;
		}
	}
}