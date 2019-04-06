/*H*
 * 
 * FILENAME: parse.c
 * DESCRIPTION: Parse programs
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 06.04.2019
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
  * This function parses a term from a stream.
  * 
  **/
Term *parser_term(FILE *stream) {
	Term *term;
	Tokenizer *tokenizer;
	Parser *state;
	int token;
	tokenizer = tokenizer_read_stream(stream);
	state = parser_expression(tokenizer, 0);
	if(!state->success) {
		token = state->next < tokenizer->nb_tokens ? state->next : tokenizer->nb_tokens-1;
		printf(
			"(parser-error (line %d) (column %d) (found \"%ls\")\n\t(message \"%ls\"))\n",
			tokenizer->tokens[token]->line,
			tokenizer->tokens[token]->column,
			state->next < tokenizer->nb_tokens ? tokenizer->tokens[token]->text : L"",
			state->error);
		term = NULL;
	} else {
		term = state->value;
	}
	parser_free(state);
	tokenizer_free(tokenizer);
	return term;
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
	int start, n_token;
	Parser *state;
	Module *module;
	Token *token;
	// Parse module declaration
	state = parser_module(tokenizer, 0);
	if(state != NULL && state->success) {
		module = state->value;
		program_add_module(program, module);
		start = state->next;
		parser_free(state);
		state = NULL;
	} else {
		module = program_get_module(program, L"user");
		start = 0;
	}
	// Parse predicates
	while(start < tokenizer->nb_tokens) {
		if(state == NULL)
			state = parser_predicate(tokenizer, start);
		if(!state->success) {
			n_token = state->next < tokenizer->nb_tokens ? state->next : tokenizer->nb_tokens-1;
			printf(
				"(parser-error (line %d) (column %d) (found \"%ls\")\n\t(message \"%ls\"))\n",
				tokenizer->tokens[n_token]->line,
				tokenizer->tokens[n_token]->column,
				state->next < tokenizer->nb_tokens ? tokenizer->tokens[n_token]->text : L"",
				state->error);
			if(state->value != NULL)
				rule_free(state->value);
			parser_free(state);
			break;
		}
		module_add_predicate(module, state->value);
		start = state->next;
		parser_free(state);
		state = NULL;
	}
}

/**
  * 
  * This function parses a module declaration.
  * 
  **/
Parser *parser_module(Tokenizer *tokenizer, int start) {
	Token *token;
	Module *module;
	Parser *state;
	// Left parenthesis
	token = tokenizer->tokens[start];
	if(token->category != TOKEN_LPAR)
		return NULL;
	// Module atom
	start++;
	token = tokenizer->tokens[start];
	if(token->category != TOKEN_ATOM || wcscmp(token->text, L"module") != 0)
		return NULL;
	state = malloc(sizeof(Parser));
	state->success = 0;
	state->start = start;
	state->value = NULL;
	// Name
	start++;
	token = tokenizer->tokens[start];
	if(token->category != TOKEN_ATOM) {
		wcscpy(state->error, L"name (an atom) expected in module declaration");
		state->next = start;
		return state;
	}
	module = module_alloc();
	module_set_name(module, token->text);
	// Right parenthesis
	start++;
	token = tokenizer->tokens[start];
	if(token->category != TOKEN_RPAR) {
		module_free(module);
		wcscpy(state->error, L"right parenthesis ')' expected at the end of module declaration");
		state->next = start;
		return state;
	}
	start++;
	start = start;
	state->value = module;
	state->success = 1;
	state->next = start;
	return state;
}

/**
  * 
  * This function parses a predicate declaration.
  * 
  **/
Parser *parser_predicate(Tokenizer *tokenizer, int start) {
	int arity, dynamic = 0, determinist = 0, local = 0;
	wchar_t *end;
	Rule *rule;
	Token *token;
	Parser *state = malloc(sizeof(Parser)), *state_expr;
	state->success = 1;
	state->start = start;
	state->next = start;
	// Tags
	token = tokenizer->tokens[start];
	while(start < tokenizer->nb_tokens && token->category == TOKEN_TAG) {
		if(wcscmp(token->text, L"det") == 0) {
			determinist = 1;
		} else if(wcscmp(token->text, L"nondet") == 0) {
			determinist = 0;
		} else if(wcscmp(token->text, L"dynamic") == 0) {
			dynamic = 1;
		} else if(wcscmp(token->text, L"static") == 0) {
			dynamic = 0;
		} else if(wcscmp(token->text, L"local") == 0) {
			local = 1;
		}
		start++;
		state->next = start;
		token = tokenizer->tokens[start];
	}
	rule = rule_alloc(dynamic, determinist, local);
	state->value = rule;
	// Left parenthesis
	if(token->category != TOKEN_LPAR) {
		wcscpy(state->error, L"predicate declaration expected");
		state->success = 0;
		return state;
	}
	// Predicate atom
	start++;
	state->next = start;
	token = tokenizer->tokens[start];
	if(token->category != TOKEN_ATOM || wcscmp(token->text, L"predicate") != 0) {
		wcscpy(state->error, L"predicate declaration expected");
		state->success = 0;
		return state;
	}
	// Name
	start++;
	state->next = start;
	token = tokenizer->tokens[start];
	if(token->category != TOKEN_ATOM) {
		wcscpy(state->error, L"name (an atom) expected in predicate declaration");
		state->success = 0;
		return state;
	}
	rule_set_name(rule, token->text);
	// Arity
	start++;
	state->next = start;
	token = tokenizer->tokens[start];
	if(token->category != TOKEN_NUMERAL) {
		wcscpy(state->error, L"arity (a non-negative numeral) expected in predicate declaration");
		state->success = 0;
		return state;
	}
	arity = wcstol(token->text, &end, 10);
	if(arity < 0) {
		wcscpy(state->error, L"arity (a non-negative numeral) expected in predicate declaration");
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
		state_expr = parser_clause(tokenizer, start, rule->name, arity);
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
		wcscpy(state->error, L"right parenthesis ')' expected at the end of predicate declaration");
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
Parser *parser_clause(Tokenizer *tokenizer, int start, wchar_t *rule_name, int arity) {
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
		wcscpy(state->error, L"clause definition or right parenthesis ')' expected");
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
		swprintf(state->error, 100, L"not enough arguments in clause definition (%d given, %d expected)", expected-arity, expected);
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
		((Term*)state_expr->value)->parent = rule_name;
		term_list_add_element(list, state_expr->value);
		parser_free(state_expr);
	}
	clause->body = list;
	// Right parenthesis
	if(token->category != TOKEN_RPAR) {
		clause_free(clause);
		term_free(list);
		wcscpy(state->error, L"right parenthesis ')' expected at the end of predicate declaration");
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
	wchar_t *end;
	Parser *state = malloc(sizeof(Parser));
	Term *term, *list;
	state->success = 1;
	state->start = start;
	state->next = start+1;
	Token *token = tokenizer->tokens[start];
	switch(token->category) {
		case TOKEN_ATOM:
			term = term_alloc();
			state->value = term;
			term->type = TYPE_ATOM;
			term_set_string(term, token->text);
			break;
		case TOKEN_CHAR:
			term = term_alloc();
			state->value = term;
			term->type = TYPE_CHAR;
			term->term.character = token->text[0];
			break;
		case TOKEN_VARIABLE:
			term = term_alloc();
			state->value = term;
			term->type = TYPE_VARIABLE;
			term_set_string(term, token->text);
			break;
		case TOKEN_NUMERAL:
			term = term_alloc();
			state->value = term;
			term->type = TYPE_NUMERAL;
			term->term.numeral = wcstol(token->text, &end, 10);
			break;
		case TOKEN_DECIMAL:
			term = term_alloc();
			state->value = term;
			term->type = TYPE_DECIMAL;
			term->term.decimal = wcstof(token->text, &end);
			break;
		case TOKEN_STRING:
			term = term_alloc();
			state->value = term;
			term->type = TYPE_STRING;
			term_set_string(term, token->text);
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
				wcscpy(state->error, L"right parenthesis ')' or expression expected");
			}
			state->next++;
			state->value = term;
			break;
		case TOKEN_ERROR:
		default:
			state->success = 0;
			wcscpy(state->error, L"unexpected token");
			state->next--;
			break;
	}
	return state;
}