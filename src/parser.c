/*H*
 * 
 * FILENAME: parse.c
 * DESCRIPTION: Parse programs
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 25.01.2019
 * 
 *H*/

#include "parser.h"



/**
  * 
  * This function ...
  * 
  **/
Program *parser_stream(FILE *stream) {
	Tokenizer *tokenizer;
	Program *program;
	tokenizer = tokenizer_read_stream(stream);
	program = parser_program(tokenizer);
	tokenizer_free(tokenizer);
	return program;
}

/**
  * 
  * This function ...
  * 
  **/
Program *parser_program(Tokenizer *tokenizer) {
	int token = 0, last_token = 0;
	ParserState *state;
	Program *program;
	program = program_alloc();
	while(token < tokenizer->nb_tokens) {
		state = parser_expression(tokenizer, token);
		last_token = token;
		token = state->start;
		if(!state->success) {
			printf(
				"(parser-error (line %d) (column %d)\n\t(message \"%s\")\n\t(token \"%s\"))\n",
				tokenizer->tokens[token]->line+1,
				tokenizer->tokens[token]->column+1,
				state->error,
				tokenizer->tokens[token]->text);
			break;
		}
		// Check command
		if(state->value->type == LIST && state->value->term.list->head->type == ATOM) {
			// Predicate declaration
			if(strcmp(state->value->term.list->head->term.string, "predicate") == 0) {
				parser_declare_predicate(program, state->value);
			// Unknown command
			} else {
				printf(
					"(unknown-command (line %d) (column %d)\n\t",
					tokenizer->tokens[last_token]->line+1,
					tokenizer->tokens[last_token]->column+1);
				term_print(state->value);
				printf(")\n");
				break;
			}
		// Unknown command
		} else {
			printf(
				"(unknown-command (line %d) (column %d)\n\t",
				tokenizer->tokens[last_token]->line+1,
				tokenizer->tokens[last_token]->column+1);
			term_print(state->value);
			printf(")\n");
			break;
		}
	}
	return program;
}

/**
  * 
  * This function ...
  * 
  **/
ParserState *parser_expression(Tokenizer *tokenizer, int token) {
	List *list, *prev = NULL;
	Term *container;
	int length = 0;
	ParserState *state = malloc(sizeof(ParserState));
	Term *term = malloc(sizeof(Term));
	state->value = term;
	state->success = 1;
	state->start = token+1;
	Token *ptr_token = tokenizer->tokens[token];
	switch(ptr_token->category) {
		case TOKEN_ATOM:
			term->type = ATOM;
			term->term.string = malloc(sizeof(char)*(ptr_token->length+1));
			strcpy(term->term.string, ptr_token->text);
			break;
		case TOKEN_VARIABLE:
			term->type = VARIABLE;
			term->term.string = malloc(sizeof(char)*(ptr_token->length+1));
			strcpy(term->term.string, ptr_token->text);
			break;
		case TOKEN_NUMBER:
			term->type = NUMERAL;
			term->term.numeral = atoi(ptr_token->text);
			break;
		case TOKEN_LPAR:
			term->type = LIST;
			term->term.list = malloc(sizeof(List));
			list = term->term.list;
			list->head = NULL;
			list->tail = NULL;
			token++;
			ptr_token = tokenizer->tokens[token];
			while(token < tokenizer->nb_tokens && ptr_token->category != TOKEN_RPAR && ptr_token->category != TOKEN_BAR) {
				free(state);
				state = parser_expression(tokenizer, token);
				if(!state->success)
					return state;
				token = state->start;
				ptr_token = tokenizer->tokens[token];
				list->head = state->value;
				state->value = term;
				container = malloc(sizeof(Term));
				container->type = LIST;
				container->term.list = malloc(sizeof(List));
				list->tail = container;
				prev = list;
				list = list->tail->term.list;
				list->head = NULL;
				list->tail = NULL;
			}
			if(token < tokenizer->nb_tokens && ptr_token->category == TOKEN_BAR) {
				state = parser_expression(tokenizer, token+1);
				if(!state->success)
					return state;
				token = state->start;
				free(prev->tail);
				prev->tail = state->value;
				state->value = term;
			}
			if(token >= tokenizer->nb_tokens || tokenizer->tokens[token]->category != TOKEN_RPAR){
				state->success = 0;
				state->start = token;
				strcpy(state->error, ") or expression expected");
			}
			state->start++;
			break;
		default:
			state->success = 0;
			strcpy(state->error, "unexpected token");
			state->start--;
			break;
	}
	return state;
}

/**
  * 
  * This function ...
  * 
  **/
int parser_declare_predicate(Program *program, Term *term) {
	return 1;
}