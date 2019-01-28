/*H*
 * 
 * FILENAME: parse.c
 * DESCRIPTION: Parse programs
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 28.01.2019
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
  * This function parses an stream and loads a program.
  * 
  **/
void parser_stream(Program *program, FILE *stream) {
	Tokenizer *tokenizer;
	tokenizer = tokenizer_read_stream(stream);
	parser_program(program, tokenizer);
	tokenizer_free(tokenizer);
}

/**
  * 
  * This function takes a tokenizer and loads a program.
  * 
  **/
void parser_program(Program *program, Tokenizer *tokenizer) {
	int start = 0;
	Parser *state;
	while(start < tokenizer->nb_tokens) {
		state = parser_predicate(tokenizer, start);
		if(!state->success) {
			printf(
				"(parser-error (line %d) (column %d) (found \"%s\")\n\t(message \"%s\"))\n",
				tokenizer->tokens[state->next]->line,
				tokenizer->tokens[state->next]->column,
				state->next < tokenizer->nb_tokens ? tokenizer->tokens[state->next]->text : "",
				state->error);
			rule_free(state->value);
			parser_free(state);
			break;
		}
		program_add_rule(program, state->value);
		start = state->next;
		parser_free(state);
	}
}

/**
  * 
  * This function parses a predicate declaration.
  * 
  **/
Parser *parser_predicate(Tokenizer *tokenizer, int start) {
	int arity, dynamic = 0, determinist = 0;
	Rule *rule;
	Token *token;
	Parser *state = malloc(sizeof(Parser)), *state_expr;
	state->success = 1;
	state->start = start;
	state->next = start;
	// Tags
	token = tokenizer->tokens[start];
	while(start < tokenizer->nb_tokens && token->category == TOKEN_TAG) {
		if(strcmp(token->text, "#det") == 0) {
			determinist = 1;
		} else if(strcmp(token->text, "#nondet") == 0) {
			determinist = 0;
		} else if(strcmp(token->text, "#dynamic") == 0) {
			dynamic = 1;
		} else if(strcmp(token->text, "#static") == 0) {
			dynamic = 0;
		}
		start++;
		state->next = start;
		token = tokenizer->tokens[start];
	}
	rule = rule_alloc(dynamic, determinist);
	state->value = rule;
	// Left parenthesis
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
	if(token->category != TOKEN_NUMERAL) {
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
	int expected = arity;
	Token *token;
	Parser *state = malloc(sizeof(Parser)), *state_expr;
	Term *list;
	Clause *clause;
	state->success = 1;
	state->start = start;
	state->next = start;
	// Left parenthesis
	token = tokenizer->tokens[start];
	if(token->category != TOKEN_LPAR) {
		strcpy(state->error, "clause definition or right parenthesis ')' expected");
		state->success = 0;
		return state;
	}
	list = term_list_empty();
	clause = clause_alloc();
	state->value = clause;
	// Head
	start++;
	state->next = start;
	token = tokenizer->tokens[start];
	while(arity > 0 && start < tokenizer->nb_tokens && token->category != TOKEN_RPAR) {
		state_expr = parser_expression(tokenizer, start);
		if(!state_expr->success) {
			clause_free(clause);
			term_free(list);
			parser_free(state);
			state_expr->value = NULL;
			return state_expr;
		}
		start = state_expr->next;
		state->next = start;
		token = tokenizer->tokens[start];
		term_list_add_element(list, state_expr->value);
		parser_free(state_expr);
		arity--;
	}
	if(arity > 0) {
		clause_free(clause);
		term_free(list);
		sprintf(state->error, "not enough arguments in clause definition (%d given, %d expected)", expected-arity, expected);
		state->success = 0;
		state->value = NULL;
		return state;
	}
	clause->head = list;
	// Body
	list = term_list_empty();
	token = tokenizer->tokens[start];
	while(start < tokenizer->nb_tokens && token->category != TOKEN_RPAR) {
		state_expr = parser_expression(tokenizer, start);
		if(!state_expr->success) {
			clause_free(clause);
			term_free(list);
			parser_free(state);
			state_expr->value = NULL;
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
		clause_free(clause);
		term_free(list);
		strcpy(state->error, "right parenthesis ')' expected at the end of predicate declaration");
		state->success = 0;
		state->value = NULL;
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
	int length = 0;
	Parser *state = malloc(sizeof(Parser));
	Term *term, *list;
	state->success = 1;
	state->start = start;
	state->next = start+1;
	Token *token = tokenizer->tokens[start];
	switch(token->category) {
		case TOKEN_ATOM:
			term = malloc(sizeof(Term));
			state->value = term;
			term->type = TYPE_ATOM;
			term->term.string = malloc(sizeof(char)*(token->length+1));
			strcpy(term->term.string, token->text);
			break;
		case TOKEN_VARIABLE:
			term = malloc(sizeof(Term));
			state->value = term;
			term->type = TYPE_VARIABLE;
			term->term.string = malloc(sizeof(char)*(token->length+1));
			strcpy(term->term.string, token->text);
			break;
		case TOKEN_NUMERAL:
			term = malloc(sizeof(Term));
			state->value = term;
			term->type = TYPE_NUMERAL;
			term->term.numeral = atoi(token->text);
			break;
		case TOKEN_DECIMAL:
			term = malloc(sizeof(Term));
			state->value = term;
			term->type = TYPE_DECIMAL;
			term->term.decimal = atof(token->text);
			break;
		case TOKEN_STRING:
			term = malloc(sizeof(Term));
			state->value = term;
			term->type = TYPE_STRING;
			term->term.string = malloc(sizeof(char)*(token->length+1));
			strcpy(term->term.string, token->text);
			break;
		case TOKEN_LPAR:
			term = term_list_empty();
			list = term;
			state->value = term;
			start++;
			token = tokenizer->tokens[start];
			while(start < tokenizer->nb_tokens && token->category != TOKEN_RPAR && token->category != TOKEN_BAR) {
				parser_free(state);
				state = parser_expression(tokenizer, start);
				if(!state->success)
					return state;
				start = state->next;
				token = tokenizer->tokens[start];
				list = term_list_add_element(list, state->value);
			}
			if(start < tokenizer->nb_tokens && token->category == TOKEN_BAR) {
				parser_free(state);
				state = parser_expression(tokenizer, start+1);
				if(!state->success)
					return state;
				start = state->next;
				term_list_set_tail(term, state->value);
			}
			if(start >= tokenizer->nb_tokens || tokenizer->tokens[start]->category != TOKEN_RPAR){
				state->success = 0;
				state->next = start;
				strcpy(state->error, "right parenthesis ')' or expression expected");
			}
			state->next++;
			state->value = term;
			break;
		case TOKEN_ERROR:
		default:
			state->success = 0;
			strcpy(state->error, "unexpected token");
			state->next--;
			break;
	}
	return state;
}