/*H*
 * 
 * FILENAME: exception.c
 * DESCRIPTION: Functions for throwing Herbrand errors
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 03.04.2019
 * 
 *H*/

#include "exception.h"



/**
  * 
  * This function generates an instantiation error returning a
  * pointer to a newly initialized Term struct.
  * 
  **/
Term *exception_instantiation_error(char *level) {
	Term *list, *error, *ins_error, *level_term;
	// Error term
	error = term_alloc();
	error->type = TYPE_ATOM;
	term_set_string(error, "error");
	// Intantiation error term
	ins_error = term_alloc();
	ins_error->type = TYPE_ATOM;
	term_set_string(ins_error, "instantiation_error");
	// Level term
	level_term = term_alloc();
	level_term->type = TYPE_ATOM;
	term_set_string(level_term, level == NULL ? "top_level" : level);
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
Term *exception_type_error(char *type, Term *found, char *level) {
	Term *list, *list_type, *error, *type_error, *expected, *level_term;
	// Error term
	error = term_alloc();
	error->type = TYPE_ATOM;
	term_set_string(error, "error");
	// Type error term
	type_error = term_alloc();
	type_error->type = TYPE_ATOM;
	term_set_string(type_error, "type_error");
	// Expected term
	expected = term_alloc();
	expected->type = TYPE_ATOM;
	term_set_string(expected, type);
	// Level term
	level_term = term_alloc();
	level_term->type = TYPE_ATOM;
	term_set_string(level_term, level == NULL ? "top_level" : level);
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
Term *exception_existence_error(char *source, Term *found, char *level) {
	Term *list, *list_existence, *error, *existence_error, *source_term, *level_term;
	// Error term
	error = term_alloc();
	error->type = TYPE_ATOM;
	term_set_string(error, "error");
	// Type error term
	existence_error = term_alloc();
	existence_error->type = TYPE_ATOM;
	term_set_string(existence_error, "existence_error");
	// Source term
	source_term = term_alloc();
	source_term->type = TYPE_ATOM;
	term_set_string(source_term, source);
	// Level term
	level_term = term_alloc();
	level_term->type = TYPE_ATOM;
	term_set_string(level_term, level == NULL ? "top_level" : level);
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