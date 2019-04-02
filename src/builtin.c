/*H*
 * 
 * FILENAME: builtin.c
 * DESCRIPTION: Functions for evaluating built-in predicates
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 02.04.2019
 * 
 *H*/

#include "builtin.h"



char *builtin_keys[BUILTIN_HASH_SIZE] = {
	NULL, ":<", NULL, NULL, NULL, NULL, "var", NULL, NULL, NULL, "<", "integer", 
	NULL, NULL, NULL, NULL, "string_length", "<=", NULL, NULL, "string_chars", NULL, 
	NULL, NULL, "list", NULL, NULL, NULL, NULL, "import", NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, ">", NULL, NULL, NULL, "current_herbrand_flag", NULL, 
	NULL, ":>", NULL, NULL, "false", "nonvar", NULL, NULL, NULL, "set_herbrand_flag", 
	"retractall", "throw", ":>=", NULL, NULL, NULL, NULL, NULL, "call", NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, "==", NULL, NULL, "succ", "number", "atom_concat", NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, "/==", NULL, NULL, NULL, "catch", "atom_chars", 
	NULL, NULL, "ground", "halt", NULL, NULL, "asserta", "and", NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, "not", NULL, NULL, NULL, NULL, NULL, "atom", NULL, NULL, 
	NULL, NULL, "assertz", NULL, NULL, "/=", "findall", NULL, NULL, "or", NULL, NULL, 
	NULL, NULL, NULL, "true", NULL, NULL, NULL, NULL, NULL, ">=", NULL, "float", 
	NULL, "is", "ite", NULL, NULL, NULL, "=", NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, "atom_length", NULL, NULL, "string", NULL, ":/==", NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, ":<=", 
	NULL, NULL, NULL, NULL, NULL, "repeat", NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "once", NULL, NULL, "!", NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, "retract", NULL, NULL, NULL, NULL, NULL, 
	NULL, "consult", "string_concat", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, ":==", NULL, NULL, NULL, NULL};

void (*builtin_handlers[BUILTIN_HASH_SIZE])() = {
	NULL, builtin_arithmetic_lt, NULL, NULL, NULL, NULL, builtin_var, NULL, NULL, 
	NULL, builtin_term_lt, builtin_integer, NULL, NULL, NULL, NULL, builtin_string_length, 
	builtin_term_le, NULL, NULL, builtin_string_chars, NULL, NULL, NULL, builtin_list, 
	NULL, NULL, NULL, NULL, builtin_import, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, builtin_term_gt, NULL, NULL, NULL, builtin_current_herbrand_flag, NULL, 
	NULL, builtin_arithmetic_gt, NULL, NULL, builtin_false, builtin_nonvar, NULL, 
	NULL, NULL, builtin_set_herbrand_flag, builtin_retractall, builtin_throw, builtin_arithmetic_ge, 
	NULL, NULL, NULL, NULL, NULL, builtin_call, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, builtin_term_eq, 
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
	NULL, builtin_once, NULL, NULL, builtin_cut, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, builtin_retract, NULL, NULL, NULL, NULL, NULL, NULL, builtin_consult, builtin_string_concat, 
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

/**
  * 
  * consult/1
  * (consult +string)
  * 
  * Read a file as a Herbrand source file.
  * (consult Path) is true when Path is a valid Herbrand source file.
  * 
  **/
void builtin_consult(Program *program, Derivation *D, State *point, Term *term) {
	Term *path;
	FILE *file;
	path = term_list_get_nth(term, 1);
	file = fopen(path->term.string, "r");
	if(file != NULL) {
		parser_stream(program, file);
		fclose(file);
		derivation_push_state(D, state_success(point, NULL));
	}
}

/**
  * 
  * import/1
  * (import +atom_or_string)
  * 
  * Load a Herbrand module.
  * (import Module) is true when Module is a valid Herbrand module.
  * 
  **/
void builtin_import(Program *program, Derivation *D, State *point, Term *term) {
	Term *path;
	FILE *file;
	char *module;
	path = term_list_get_nth(term, 1);
	module = malloc(sizeof(char)*(34+strlen(path->term.string)));
	sprintf(module, "/usr/local/herbrand/modules/%s.hb", path->term.string);
	file = fopen(module, "r");
	free(module);
	if(file != NULL) {
		parser_stream(program, file);
		fclose(file);
		derivation_push_state(D, state_success(point, NULL));
	}
}

/**
  * 
  * and/2
  * (and +callable_term +callable_term)
  * 
  * Conjunction.
  * (and First Second) is true if and only if First is true and Second is true.
  * 
  **/
void builtin_and(Program *program, Derivation *D, State *point, Term *term) {
	Term *tail;
	tail = term_list_get_tail(term);
	derivation_push_state(D, state_success(point, tail));
}

/**
  * 
  * or/2
  * (or +callable_term +callable_term)
  * 
  * Disjunction.
  * (or Either Or) is true if and only if either Either or Or is true.
  * 
  **/
void builtin_or(Program *program, Derivation *D, State *point, Term *term) {
	Term *left, *right;
	left = term_list_get_nth(term, 1);
	right = term_list_get_nth(term, 2);
	// Right goal
	term_increase_references(right);
	derivation_push_state(D, state_success(point, right));
	// Left goal
	term_increase_references(left);
	derivation_push_state(D, state_success(point, left));
}

void builtin_ite(Program *program, Derivation *D, State *point, Term *term) {
	Term *list, *cut, *list_cut, *cond, *then, *els;
	cond = term_list_get_nth(term, 1);
	then = term_list_get_nth(term, 2);
	els = term_list_get_nth(term, 3);
	// Cut term
	cut = term_alloc();
	cut->type = TYPE_ATOM;
	term_set_string(cut, "!");
	// Cut call
	list_cut = term_list_empty();
	term_list_add_element(list_cut, cut);
	// (If (!) Then)
	list = term_list_empty();
	term_list_add_element(list, cond);
	term_list_add_element(list, list_cut);
	term_list_add_element(list, then);
	term_increase_references(cond);
	term_increase_references(then);
	derivation_push_state(D, state_success(point, els));
	derivation_push_state(D, state_success(point, list));
	term_free(list);
}

void builtin_cut(Program *program, Derivation *D, State *point, Term *term) {
	
}

void builtin_not(Program *program, Derivation *D, State *point, Term *term) {
	
}

void builtin_call(Program *program, Derivation *D, State *point, Term *term) {
	Term *tail, *goal;
	goal = term_list_clone(term_list_get_nth(term, 1));
	tail = term_list_get_tail(term);
	tail = term_list_get_tail(tail);
	term_list_set_tail(goal, tail);
	term_increase_references(tail);
	derivation_push_state(D, state_success(point, goal));
	term_free(goal);
}

void builtin_once(Program *program, Derivation *D, State *point, Term *term) {
	Term *list, *cut, *call, *list_cut, *goal, *list_goal;
	goal = term_list_get_nth(term, 1);
	// Cut term
	cut = term_alloc();
	cut->type = TYPE_ATOM;
	term_set_string(cut, "!");
	// Call term
	call = term_alloc();
	call->type = TYPE_ATOM;
	term_set_string(call, "call");
	// Cut call
	list_cut = term_list_empty();
	term_list_add_element(list_cut, cut);
	// Goal call
	list_goal = term_list_empty();
	term_list_add_element(list_goal, call);
	term_list_add_element(list_goal, goal);
	term_increase_references(goal);
	// ((call Goal) (!))
	list = term_list_empty();
	term_list_add_element(list, list_goal);
	term_list_add_element(list, list_cut);
	derivation_push_state(D, state_success(point, list));
	term_free(list);
}

/**
  * 
  * repeat/0
  * (repeat)
  * 
  * Provide infinite choice points.
  * (repeat) is true. It provides infinite choice points, what makes it perfect
  * for creating loops.
  * 
  **/
void builtin_repeat(Program *program, Derivation *D, State *point, Term *term) {
	State *state = state_alloc();
	state->goal = point->goal;
	term_increase_references(point->goal);
	state->substitution = point->substitution;
	substitution_increase_references(point->substitution);
	state->parent = point;
	derivation_push_state(D, state);
	derivation_push_state(D, state_success(point, NULL));
}

/**
  * 
  * true/0
  * (true)
  * 
  * Alwais succeed.
  * 
  **/
void builtin_true(Program *program, Derivation *D, State *point, Term *term) {
	derivation_push_state(D, state_success(point, NULL));
}

/**
  * 
  * false/0
  * (false)
  * 
  * Alwais fail.
  * 
  **/
void builtin_false(Program *program, Derivation *D, State *point, Term *term) {
}

void builtin_catch(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_throw(Program *program, Derivation *D, State *point, Term *term) {
}

/**
  * 
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

/**
  * 
  * (/=)/2
  * (/= ?term ?term)
  * 
  * Not unification.
  * (/= X Y) is true if and only if X and Y are not unifiable. True if the unification fails.
  * 
  **/
void builtin_not_unifcation(Program *program, Derivation *D, State *point, Term *term) {
	Substitution *mgu;
	Term *left, *right;
	left = term_list_get_nth(term, 1);
	right = term_list_get_nth(term, 2);
	mgu = semantics_unify_terms(left, right, 0);
	if(mgu == NULL) 
		derivation_push_state(D, state_success(point, NULL));
	else
		substitution_free(mgu);
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

/**
  * 
  * atom/1
  * (atom @term)
  * 
  * Check if atom.
  * (atom X) is true if and only if X is an atom.
  * 
  **/
void builtin_atom(Program *program, Derivation *D, State *point, Term *term) {
	Term *atom = term_list_get_nth(term, 1);
	if(atom->type == TYPE_ATOM) 
		derivation_push_state(D, state_success(point, NULL));
}

/**
  * 
  * number/1
  * (number @term)
  * 
  * Check if number.
  * (number X) is true if and only if X is a number.
  * 
  **/
void builtin_number(Program *program, Derivation *D, State *point, Term *term) {
	Term *number = term_list_get_nth(term, 1);
	if(number->type == TYPE_NUMERAL || number->type == TYPE_DECIMAL) 
		derivation_push_state(D, state_success(point, NULL));
}

/**
  * 
  * integer/1
  * (integer @term)
  * 
  * Check if integer.
  * (integer X) is true if and only if X is an integer.
  * 
  **/
void builtin_integer(Program *program, Derivation *D, State *point, Term *term) {
	Term *integer = term_list_get_nth(term, 1);
	if(integer->type == TYPE_NUMERAL)
		derivation_push_state(D, state_success(point, NULL));
}

/**
  * 
  * float/1
  * (float @term)
  * 
  * Check if float.
  * (float X) is true if and only if X is a float.
  * 
  **/
void builtin_float(Program *program, Derivation *D, State *point, Term *term) {
	Term *decimal = term_list_get_nth(term, 1);
	if(decimal->type == TYPE_DECIMAL)
		derivation_push_state(D, state_success(point, NULL));
}

/**
  * 
  * string/1
  * (string @term)
  * 
  * Check if string.
  * (string X) is true if and only if X is a string.
  * 
  **/
void builtin_string(Program *program, Derivation *D, State *point, Term *term) {
	Term *string = term_list_get_nth(term, 1);
	if(string->type == TYPE_STRING)
		derivation_push_state(D, state_success(point, NULL));
}

/**
  * 
  * ground/1
  * (ground @term)
  * 
  * Check if ground term.
  * (ground Term) is true if and only if Term holds no free variables.
  * 
  **/
void builtin_ground(Program *program, Derivation *D, State *point, Term *term) {
	int nb_vars = 0;
	Term *ground = term_list_get_nth(term, 1);
	Term **vars = term_get_variables(ground, &nb_vars);
	free(vars);
	if(nb_vars == 0)
		derivation_push_state(D, state_success(point, NULL));
}

/**
  * 
  * list/1
  * (list @term)
  * 
  * Check if list.
  * (list X) is true if and only if X is a list.
  * 
  **/
void builtin_list(Program *program, Derivation *D, State *point, Term *term) {
	Term *list = term_list_get_nth(term, 1);
	if(list->type == TYPE_LIST)
		derivation_push_state(D, state_success(point, NULL));
}

/**
  * 
  * var/1
  * (var @term)
  * 
  * Check if variable.
  * (var X) is true if and only if X is a variable.
  * 
  **/
void builtin_var(Program *program, Derivation *D, State *point, Term *term) {
	Term *var = term_list_get_nth(term, 1);
	if(var->type == TYPE_VARIABLE)
		derivation_push_state(D, state_success(point, NULL));
}

/**
  * 
  * nonvar/1
  * (nonvar @term)
  * 
  * Check if not variable.
  * (nonvar X) is true if and only if X is not a variable.
  * 
  **/
void builtin_nonvar(Program *program, Derivation *D, State *point, Term *term) {
	Term *nonvar = term_list_get_nth(term, 1);
	if(nonvar->type != TYPE_VARIABLE)
		derivation_push_state(D, state_success(point, NULL));
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

/**
  * 
  * halt/1
  * (halt +number)
  * 
  * Terminate a Herbrand processor and return message.
  * (halt X) exits the processor and returns to the system that invoked
  * the processor, passing a message through the X variable.
  * 
  **/
void builtin_halt(Program *program, Derivation *D, State *point, Term *term) {
	Term *message = term_list_get_nth(term, 1);
	exit(message->term.numeral);
}

void builtin_current_herbrand_flag(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_set_herbrand_flag(Program *program, Derivation *D, State *point, Term *term) {
}
