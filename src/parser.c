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
  * This function frees a previously allocated state.
  * The terms underlying the rule will not be deallocated.
  * 
  **/
void parser_free(Parser *state) {
	free(state);
}

/**
  * 
  * This function parses an stream and returns a program.
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
  * This function takes a tokenizer and returns a program.
  * 
  **/
Program *parser_program(Tokenizer *tokenizer) {
	int start = 0;
	Parser *state;
	Program *program = program_alloc();
	while(start < tokenizer->nb_tokens) {
		state = parser_predicate(tokenizer, start);
		if(!state->success)
			break;
		else
			program_add_rule(program, state->value);
		start = state->next;
	}
	return program;
}

/**
  * 
  * This function parses a predicate declaration.
  * 
  **/
Parser *parser_predicate(Tokenizer *tokenizer, int start) {
	int arity;
	Rule *rule;
	Token *token;
	Parser *state = malloc(sizeof(Parser)), *state_expr;
	state->success = 1;
	state->start = start;
	state->next = start;
	rule = rule_alloc(0);
	state->value = rule;
	// Left parenthesis
	token = tokenizer->tokens[start];
	if(token->category != TOKEN_LPAR) {
		strcpy(state->error, "predicate declaration expected");
		state->success = 0;
		return state;
	}
	// Predicate atom
	start++;
	state->next = start;
	token = tokenizer->tokens[start];
	if(token->category != TOKEN_ATOM || strcmp(token->text, "predicate") != 0) {
		strcpy(state->error, "predicate declaration expected");
		state->success = 0;
		return state;
	}
	// Name
	start++;
	state->next = start;
	token = tokenizer->tokens[start];
	if(token->category != TOKEN_ATOM) {
		strcpy(state->error, "name (an atom) expected in predicate declaration");
		state->success = 0;
		return state;
	}
	rule_set_name(rule, token->text);
	// Arity
	start++;
	state->next = start;
	token = tokenizer->tokens[start];
	if(token->category != TOKEN_NUMBER) {
		strcpy(state->error, "arity (a non-negative numeral) expected in predicate declaration");
		state->success = 0;
		return state;
	}
	arity = atoi(token->text);
	if(arity < 0) {
		strcpy(state->error, "arity (a non-negative numeral) expected in predicate declaration");
		state->success = 0;
		return state;
	}
	rule->arity = arity;
	// Type
	start++;
	state->next = start;
	state_expr = parser_expression(tokenizer, start);
	if(!state_expr->success) {
		parser_free(state);
		state_expr->value = rule;
		return state_expr;
	}
	rule->type = state_expr->value;
	// Clauses
	start = state_expr->next;
	parser_free(state_expr);
	state->next = start;
	token = tokenizer->tokens[start];
	while(start < tokenizer->nb_tokens && token->category != TOKEN_RPAR) {
		state_expr = parser_clause(tokenizer, start, arity);
		if(!state_expr->success) {
			parser_free(state);
			state_expr->value = rule;
			return state_expr;
		}
		start = state_expr->next;
		state->next = start;
		token = tokenizer->tokens[start];
		rule_add_clause(rule, state_expr->value);
		parser_free(state_expr);
	}
	// Right parenthesis
	if(token->category != TOKEN_RPAR) {
		strcpy(state->error, "right parenthesis ')' expected at the end of predicate declaration");
		state->success = 0;
		return state;
	}
	start++;
	state->next = start;
	return state;
}

/**
  * 
  * This function parses a clause of a predicate declaration.
  * 
  **/
Parser *parser_clause(Tokenizer *tokenizer, int start, int arity) {
	Token *token;
	Parser *state = malloc(sizeof(Parser)), *state_expr;
	Term *list = term_list_empty();
	Clause *clause = clause_alloc();
	state->success = 1;
	state->start = start;
	state->next = start;
	state->value = clause;
	// Left parenthesis
	token = tokenizer->tokens[start];
	if(token->category != TOKEN_LPAR) {
		strcpy(state->error, "predicate declaration expected");
		state->success = 0;
		return state;
	}
	// Head
	start++;
	state->next = start;
	token = tokenizer->tokens[start];
	while(arity > 0 && start < tokenizer->nb_tokens) {
		state_expr = parser_expression(tokenizer, start);
		if(!state_expr->success) {
			parser_free(state);
			state_expr->value = clause;
			return state_expr;
		}
		start = state_expr->next;
		state->next = start;
		term_list_add_element(list, state_expr->value);
		parser_free(state_expr);
		arity--;
	}
	clause->head = list;
	if(arity > 0) {
		strcpy(state->error, "not enough arguments in clause definition");
		state->success = 0;
		return state;
	}
	// Body
	list = term_list_empty();
	token = tokenizer->tokens[start];
	while(start < tokenizer->nb_tokens && token->category != TOKEN_RPAR) {
		state_expr = parser_expression(tokenizer, start);
		if(!state_expr->success) {
			parser_free(state);
			state_expr->value = clause;
			return state_expr;
		}
		start = state_expr->next;
		state->next = start;
		token = tokenizer->tokens[start];
		term_list_add_element(list, state_expr->value);
		parser_free(state_expr);
	}
	clause->body = list;
	// Right parenthesis
	if(token->category != TOKEN_RPAR) {
		strcpy(state->error, "right parenthesis ')' expected at the end of predicate declaration");
		state->success = 0;
		return state;
	}
	start++;
	state->next = start;
	return state;
}

/**
  * 
  * This function parses an expression.
  * 
  **/
Parser *parser_expression(Tokenizer *tokenizer, int start) {
	List *list, *prev = NULL;
	Term *container;
	int length = 0;
	Parser *state = malloc(sizeof(Parser));
	Term *term = malloc(sizeof(Term));
	state->value = term;
	state->success = 1;
	state->start = start;
	state->next = start+1;
	Token *token = tokenizer->tokens[start];
	switch(token->category) {
		case TOKEN_ATOM:
			term->type = TYPE_ATOM;
			term->term.string = malloc(sizeof(char)*(token->length+1));
			strcpy(term->term.string, token->text);
			break;
		case TOKEN_VARIABLE:
			term->type = TYPE_VARIABLE;
			term->term.string = malloc(sizeof(char)*(token->length+1));
			strcpy(term->term.string, token->text);
			break;
		case TOKEN_NUMBER:
			term->type = TYPE_NUMERAL;
			term->term.numeral = atoi(token->text);
			break;
		case TOKEN_LPAR:
			term->type = TYPE_LIST;
			term->term.list = malloc(sizeof(List));
			list = term->term.list;
			list->head = NULL;
			list->tail = NULL;
			start++;
			token = tokenizer->tokens[start];
			while(start < tokenizer->nb_tokens && token->category != TOKEN_RPAR && token->category != TOKEN_BAR) {
				free(state);
				state = parser_expression(tokenizer, start);
				if(!state->success)
					return state;
				start = state->next;
				token = tokenizer->tokens[start];
				list->head = state->value;
				state->value = term;
				container = malloc(sizeof(Term));
				container->type = TYPE_LIST;
				container->term.list = malloc(sizeof(List));
				list->tail = container;
				prev = list;
				list = list->tail->term.list;
				list->head = NULL;
				list->tail = NULL;
			}
			if(start < tokenizer->nb_tokens && token->category == TOKEN_BAR) {
				state = parser_expression(tokenizer, start+1);
				if(!state->success)
					return state;
				start = state->next;
				free(prev->tail);
				prev->tail = state->value;
				state->value = term;
			}
			if(start >= tokenizer->nb_tokens || tokenizer->tokens[start]->category != TOKEN_RPAR){
				state->success = 0;
				state->next = start;
				strcpy(state->error, "right parenthesis ')' or expression expected");
			}
			state->next++;
			break;
		default:
			state->success = 0;
			strcpy(state->error, "unexpected token");
			state->next--;
			break;
	}
	return state;
}