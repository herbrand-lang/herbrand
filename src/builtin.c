/*H*
 * 
 * FILENAME: builtin.c
 * DESCRIPTION: Functions for evaluating built-in predicates
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 31.03.2019
 * 
 *H*/

#include "builtin.h"



char *builtin_keys[BUILTIN_HASH_SIZE] = {
	NULL, ":<", NULL, NULL, NULL, NULL, "var", NULL, NULL, NULL, "<", "integer", 
	NULL, NULL, NULL, NULL, "string_length", "<=", NULL, NULL, "string_chars", NULL, 
	NULL, NULL, "list", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, ">", NULL, NULL, NULL, "current_herbrand_flag", NULL, NULL, 
	":>", NULL, NULL, "false", "nonvar", NULL, NULL, NULL, "set_herbrand_flag", "retractall", 
	"throw", ":>=", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "==", 
	NULL, NULL, "succ", "number", "atom_concat", NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, "/==", NULL, NULL, NULL, "catch", "atom_chars", NULL, 
	NULL, "ground", "halt", NULL, NULL, "asserta", "and", NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, "not", NULL, NULL, NULL, NULL, NULL, "atom", NULL, NULL, NULL, 
	NULL, "assertz", NULL, NULL, "/=", "findall", NULL, NULL, "or", NULL, NULL, NULL, 
	NULL, NULL, "true", NULL, NULL, NULL, NULL, NULL, ">=", NULL, "float", NULL, 
	"is", "ite", NULL, NULL, NULL, "=", NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, "atom_length", NULL, NULL, "string", NULL, ":/==", NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, ":<=", NULL, 
	NULL, NULL, NULL, NULL, "repeat", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, "once", NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, "retract", NULL, NULL, NULL, NULL, NULL, NULL, 
	"consult", "string_concat", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, ":==", NULL, NULL, NULL, NULL};

void (*builtin_handlers[BUILTIN_HASH_SIZE])() = {
	NULL, builtin_arithmetic_lt, NULL, NULL, NULL, NULL, builtin_var, NULL, NULL, 
	NULL, builtin_term_lt, builtin_integer, NULL, NULL, NULL, NULL, builtin_string_length, 
	builtin_term_le, NULL, NULL, builtin_string_chars, NULL, NULL, NULL, builtin_list, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	builtin_term_gt, NULL, NULL, NULL, builtin_current_herbrand_flag, NULL, NULL, 
	builtin_arithmetic_gt, NULL, NULL, builtin_false, builtin_nonvar, NULL, NULL, 
	NULL, builtin_set_herbrand_flag, builtin_retractall, builtin_throw, builtin_arithmetic_ge, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, builtin_term_eq, 
	NULL, NULL, builtin_succ, builtin_number, builtin_atom_concat, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, builtin_term_ne, NULL, NULL, NULL, 
	builtin_catch, builtin_atom_chars, NULL, NULL, builtin_ground, builtin_halt, 
	NULL, NULL, builtin_asserta, builtin_and, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, builtin_not, NULL, NULL, NULL, NULL, NULL, builtin_atom, NULL, NULL, NULL, 
	NULL, builtin_assertz, NULL, NULL, builtin_not_unifcation, builtin_findall, NULL, 
	NULL, builtin_or, NULL, NULL, NULL, NULL, NULL, builtin_true, NULL, NULL, NULL, 
	NULL, NULL, builtin_term_ge, NULL, builtin_float, NULL, builtin_is, builtin_ite, 
	NULL, NULL, NULL, builtin_unification, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, builtin_atom_length, NULL, NULL, builtin_string, NULL, builtin_arithmetic_ne, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, builtin_arithmetic_le, NULL, NULL, NULL, NULL, NULL, builtin_repeat, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, builtin_once, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	builtin_retract, NULL, NULL, NULL, NULL, NULL, NULL, builtin_consult, builtin_string_concat, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, builtin_arithmetic_eq, 
	NULL, NULL, NULL, NULL};



/**
  * 
  * This functions cheks if an atom is a built-in
  * predicate.
  * 
  **/
int builtin_check_predicate(Term *term) {
	int key = hashmap_function(BUILTIN_HASH_SIZE, term->term.string);
	return builtin_keys[key] != NULL && strcmp(term->term.string, builtin_keys[key]) == 0;
}

/**
  * 
  * This functions runs a built-in predicate.
  * 
  **/
int builtin_run_predicate(Program *program, Derivation *D, State *point, Term *term) {
	Term *head = term_list_get_head(term);
	int key = hashmap_function(BUILTIN_HASH_SIZE, head->term.string);
	if(builtin_keys[key] != NULL && strcmp(head->term.string, builtin_keys[key]) == 0) {
		builtin_handlers[key](program, D, point, term);
		return 1;
	}
	return 0;
}

void builtin_consult(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_and(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_or(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_ite(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_not(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_once(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_repeat(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_true(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_false(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_catch(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_throw(Program *program, Derivation *D, State *point, Term *term) {
}

/**
  * (=)/2
  * (= ?term ?term)
  * 
  * Unification.
  * (= X Y) is true if and only if X and Y are unifiable. True if the unification succeeds.
  * 
  **/
void builtin_unification(Program *program, Derivation *D, State *point, Term *term) {
	Term *list;
	State *state;
	Substitution *mgu;
	Term *left, *right;
	left = term_list_get_nth(term, 1);
	right = term_list_get_nth(term, 2);
	mgu = semantics_unify_terms(left, right, 0);
	if(mgu != NULL) {
		list = term_list_empty();
		state = state_inference(point, list, mgu);
		term_free(list);
		derivation_push_state(D, state);
		substitution_free(mgu);
	}
}

void builtin_not_unifcation(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_asserta(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_assertz(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_retract(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_retractall(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_term_eq(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_term_ne(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_term_lt(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_term_le(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_term_gt(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_term_ge(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_arithmetic_eq(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_arithmetic_ne(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_arithmetic_lt(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_arithmetic_le(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_arithmetic_gt(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_arithmetic_ge(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_atom(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_number(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_integer(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_float(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_string(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_ground(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_list(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_var(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_nonvar(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_atom_length(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_atom_concat(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_atom_chars(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_string_length(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_string_concat(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_string_chars(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_findall(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_is(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_succ(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_halt(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_current_herbrand_flag(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_set_herbrand_flag(Program *program, Derivation *D, State *point, Term *term) {
}
