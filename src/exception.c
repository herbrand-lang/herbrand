/*H*
 * 
 * FILENAME: exception.c
 * DESCRIPTION: Functions for throwing Herbrand errors
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 06.04.2019
 * 
 *H*/

#include "exception.h"



/**
  * 
  * This function generates an instantiation error returning a
  * pointer to a newly initialized Term struct.
  * 
  **/
Term *exception_instantiation_error(wchar_t *level) {
	Term *list, *error, *ins_error, *level_term;
	error = term_init_atom(L"error");
	ins_error = term_init_atom(L"instantiation_error");
	// Level term
	level_term = term_alloc();
	level_term->type = TYPE_ATOM;
	term_set_string(level_term, level == NULL ? L"top_level" : level);
	// Error term
	list = term_list_empty();
	term_list_add_element(list, error);
	term_list_add_element(list, ins_error);
	term_list_add_element(list, level_term);
	return list;
}

/**
  * 
  * This function generates a type error returning a pointer
  * to a newly initialized Term struct.
  * 
  **/
Term *exception_type_error(wchar_t *type, Term *found, wchar_t *level) {
	Term *list, *list_type, *error, *type_error, *expected, *level_term;
	error = term_init_atom(L"error");
	type_error = term_init_atom(L"type_error");
	expected = term_init_atom(type);
	// Level term
	level_term = term_alloc();
	level_term->type = TYPE_ATOM;
	term_set_string(level_term, level == NULL ? L"top_level" : level);
	// Found term
	term_increase_references(found);
	// Error term
	list_type = term_list_empty();
	term_list_add_element(list_type, type_error);
	term_list_add_element(list_type, expected);
	term_list_add_element(list_type, found);
	list = term_list_empty();
	term_list_add_element(list, error);
	term_list_add_element(list, list_type);
	term_list_add_element(list, level_term);
	return list;
}

/**
  * 
  * This function generates an existence error returning a
  * pointer to a newly initialized Term struct.
  * 
  **/
Term *exception_existence_error(wchar_t *source, Term *found, wchar_t *level) {
	Term *list, *list_existence, *error, *existence_error, *source_term, *level_term;
	error = term_init_atom(L"error");
	existence_error = term_init_atom(L"existence_error");
	source_term = term_init_atom(source);
	// Level term
	level_term = term_alloc();
	level_term->type = TYPE_ATOM;
	term_set_string(level_term, level == NULL ? L"top_level" : level);
	// Found term
	term_increase_references(found);
	// Error term
	list_existence = term_list_empty();
	term_list_add_element(list_existence, existence_error);
	term_list_add_element(list_existence, source_term);
	term_list_add_element(list_existence, found);
	list = term_list_empty();
	term_list_add_element(list, error);
	term_list_add_element(list, list_existence);
	term_list_add_element(list, level_term);
	return list;
}

/**
  * 
  * This function generates an arity error returning a
  * pointer to a newly initialized Term struct.
  * 
  **/
Term *exception_arity_error(int arity, int given, Term *found, wchar_t *level) {
	Term *list, *list_arity, *list_given, *list_expected, *error, *arity_error,
		*expected_term, *given_term, *expected_n, *given_n, *level_term;
	error = term_init_atom(L"error");
	arity_error = term_init_atom(L"arity_error");
	expected_term = term_init_atom(L"expected");
	given_term = term_init_atom(L"given");
	expected_n = term_init_numeral(arity);
	given_n = term_init_numeral(given);
	// (expected Arity)
	list_expected = term_list_empty();
	term_list_add_element(list_expected, expected_term);
	term_list_add_element(list_expected, expected_n);
	// (given Length)
	list_given = term_list_empty();
	term_list_add_element(list_given, given_term);
	term_list_add_element(list_given, given_n);
	// Level term
	level_term = term_alloc();
	level_term->type = TYPE_ATOM;
	term_set_string(level_term, level == NULL ? L"top_level" : level);
	// Found term
	term_increase_references(found);
	// Error term
	list_arity = term_list_empty();
	term_list_add_element(list_arity, arity_error);
	term_list_add_element(list_arity, list_expected);
	term_list_add_element(list_arity, list_given);
	term_list_add_element(list_arity, found);
	list = term_list_empty();
	term_list_add_element(list, error);
	term_list_add_element(list, list_arity);
	term_list_add_element(list, level_term);
	return list;
}