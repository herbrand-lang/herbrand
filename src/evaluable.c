/*H*
 * 
 * FILENAME: evaluable.c
 * DESCRIPTION: Functions for evaluating arithmetic terms
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 07.04.2019
 * 
 *H*/

#include "evaluable.h"



wchar_t *evaluable_keys[EVALUABLE_HASH_SIZE] = {
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, L"tau", NULL, 
	L"+", NULL, NULL, NULL, NULL, NULL, NULL, L"/", L"pi", NULL, NULL, L"-", NULL, 
	L"e", L"*", NULL, NULL, NULL, NULL};

Term *(*evaluable_handlers[EVALUABLE_HASH_SIZE])() = {
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, evaluable_tau, 
	NULL, evaluable_add, NULL, NULL, NULL, NULL, NULL, NULL, evaluable_div, evaluable_pi, 
	NULL, NULL, evaluable_sub, NULL, evaluable_e, evaluable_mul, NULL, NULL, NULL, 
	NULL};

int evaluable_arities[EVALUABLE_HASH_SIZE] = {
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, -1, 
	0, 0, -1, 0, 0, 0, 0};



/**
  * 
  * This function cheks if an atom is a evaluable
  * term.
  * 
  **/
int evaluable_check_term(Term *term) {
	int key = hashmap_function(EVALUABLE_HASH_SIZE, term->term.string);
	return evaluable_keys[key] != NULL && wcscmp(term->term.string, evaluable_keys[key]) == 0;
}

/**
  * 
  * This function evaluates an evaluable term.
  * 
  **/
Term *evaluable_eval_term(Term *term) {
	Term *error, *head, *list, *eval;
	int arity, length, key;
	if(term->type == TYPE_NUMERAL || term->type == TYPE_DECIMAL) {
		term_increase_references(term);
		return term;
	}
	head = term_list_get_head(term);
	key = hashmap_function(EVALUABLE_HASH_SIZE, head->term.string);
	if(evaluable_keys[key] != NULL && wcscmp(head->term.string, evaluable_keys[key]) == 0) {
		arity = evaluable_arities[key];
		length = term_list_length(term)-1;
		if(length == -1) {
			return exception_type_error(L"callable_term", term, term->parent);
		} else if(length == arity || arity == -1) {
			list = term_list_empty();
			term_list_add_element(list, head);
			term_increase_references(head);
			term = term->term.list->tail;
			while(term->type == TYPE_LIST && !term_list_is_null(term)) {
				eval = evaluable_eval_term(term->term.list->head);
				term_list_add_element(list, eval);
				term = term->term.list->tail;
			}
			eval = evaluable_handlers[key](list);
			term_free(list);
			return eval;
		} else {
			return exception_arity_error(arity, length, term, term->parent);
		}
	}
	return NULL;
}

/**
  * 
  * pi/0
  * (pi)
  * 
  * PI constant.
  * 
  **/
Term *evaluable_pi(Term *term) {
	return term_init_decimal(EVALUABLE_PI);
}

/**
  * 
  * e/0
  * (e)
  * 
  * E constant.
  * 
  **/
Term *evaluable_e(Term *term) {
	return term_init_decimal(EVALUABLE_E);
}

/**
  * 
  * tau/0
  * (tau)
  * 
  * TAU constant.
  * 
  **/
Term *evaluable_tau(Term *term) {
	return term_init_decimal(EVALUABLE_TAU);
}

/**
  * 
  * (+)/*
  * (+ +term+)
  * 
  * Addition.
  * 
  **/
Term *evaluable_add(Term *term) {
	Term *head;
	int is_float = 0;
	double sum = 0;
	term = term->term.list->tail;
	while(!term_list_is_null(term)) {
		head = term->term.list->head;
		is_float = is_float || head->type == TYPE_DECIMAL;
		sum += head->type == TYPE_DECIMAL ? head->term.decimal : head->term.numeral;
		term = term->term.list->tail;
	}
	if(is_float)
		return term_init_decimal(sum);
	else
		return term_init_numeral(floor(sum));
}

/**
  * 
  * (-)/*
  * (- +term+)
  * 
  * Substraction.
  * 
  **/
Term *evaluable_sub(Term *term) {
	Term *head;
	int is_float;
	double sum;
	term = term->term.list->tail;
	head = term->term.list->head;
	is_float = head->type == TYPE_DECIMAL;
	sum = is_float ? head->term.decimal : head->term.numeral;
	term = term->term.list->tail;
	while(!term_list_is_null(term)) {
		head = term->term.list->head;
		is_float = is_float || head->type == TYPE_DECIMAL;
		sum -= head->type == TYPE_DECIMAL ? head->term.decimal : head->term.numeral;
		term = term->term.list->tail;
	}
	if(is_float)
		return term_init_decimal(sum);
	else
		return term_init_numeral(floor(sum));
}

/**
  * 
  * (*)/*
  * (* +term*)
  * 
  * Multiplication.
  * 
  **/
Term *evaluable_mul(Term *term) {
	Term *head;
	int is_float = 0;
	double prod = 1;
	term = term->term.list->tail;
	while(!term_list_is_null(term)) {
		head = term->term.list->head;
		is_float = is_float || head->type == TYPE_DECIMAL;
		prod *= head->type == TYPE_DECIMAL ? head->term.decimal : head->term.numeral;
		term = term->term.list->tail;
	}
	if(is_float)
		return term_init_decimal(prod);
	else
		return term_init_numeral(floor(prod));
}

/**
  * 
  * (/)/*
  * (/ +term+)
  * 
  * Division.
  * 
  **/
Term *evaluable_div(Term *term) {}