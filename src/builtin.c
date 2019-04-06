/*H*
 * 
 * FILENAME: builtin.c
 * DESCRIPTION: Functions for evaluating built-in predicates
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 06.04.2019
 * 
 *H*/

#include "builtin.h"



wchar_t *builtin_keys[BUILTIN_HASH_SIZE] = {
	NULL, L":<", NULL, NULL, NULL, NULL, L"var", NULL, NULL, NULL, L"<", L"integer", 
	NULL, NULL, NULL, NULL, L"string_length", L"<=", NULL, NULL, L"string_chars", 
	NULL, NULL, NULL, L"list", NULL, NULL, NULL, NULL, L"import", NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, L">", NULL, NULL, NULL, L"current_herbrand_flag", 
	NULL, NULL, L":>", NULL, NULL, L"false", L"nonvar", NULL, NULL, NULL, L"set_herbrand_flag", 
	L"retractall", L"throw", L":>=", NULL, NULL, NULL, NULL, NULL, L"call", NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, L"==", NULL, NULL, L"succ", L"number", L"atom_concat", NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, L"/==", NULL, NULL, NULL, 
	L"catch", L"atom_chars", NULL, NULL, L"ground", L"halt", NULL, NULL, L"asserta", 
	L"and", NULL, NULL, NULL, NULL, NULL, NULL, NULL, L"not", NULL, NULL, NULL, NULL, 
	NULL, L"atom", NULL, NULL, NULL, NULL, L"assertz", NULL, NULL, L"/=", L"findall", 
	NULL, NULL, L"or", NULL, NULL, NULL, NULL, NULL, L"true", NULL, NULL, NULL, NULL, 
	NULL, L">=", NULL, L"float", NULL, L"is", L"ite", NULL, NULL, NULL, L"=", NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, L"atom_length", NULL, NULL, L"string", 
	NULL, L":/==", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, L":<=", NULL, NULL, NULL, NULL, NULL, L"repeat", NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, L"$catcher", NULL, NULL, NULL, NULL, 
	NULL, L"once", NULL, NULL, L"!", NULL, NULL, NULL, NULL, NULL, NULL, NULL, L"retract", 
	NULL, NULL, NULL, NULL, NULL, NULL, L"consult", L"string_concat", NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, L":==", NULL, NULL, NULL, NULL};

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
	NULL, builtin_assertz, NULL, NULL, builtin_not_unification, builtin_findall, 
	NULL, NULL, builtin_or, NULL, NULL, NULL, NULL, NULL, builtin_true, NULL, NULL, 
	NULL, NULL, NULL, builtin_term_ge, NULL, builtin_float, NULL, builtin_is, builtin_ite, 
	NULL, NULL, NULL, builtin_unification, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, builtin_atom_length, NULL, NULL, builtin_string, NULL, builtin_arithmetic_ne, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, builtin_arithmetic_le, NULL, NULL, NULL, NULL, NULL, builtin_repeat, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, builtin__catcher, NULL, NULL, 
	NULL, NULL, NULL, builtin_once, NULL, NULL, builtin_cut, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, builtin_retract, NULL, NULL, NULL, NULL, NULL, NULL, builtin_consult, 
	builtin_string_concat, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, builtin_arithmetic_eq, NULL, NULL, NULL, NULL};

int builtin_arities[BUILTIN_HASH_SIZE] = {
	0, 2, 0, 0, 0, 0, 1, 0, 0, 0, 2, 1, 0, 0, 0, 0, 2, 2, 0, 0, 2, 0, 0, 0, 1, 0, 
	0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 1, 0, 0, 
	0, 2, 1, 1, 2, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 2, 0, 0, 2, 1, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 2, 0, 0, 
	1, 1, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 
	0, 2, 3, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 1, 0, 2, 3, 0, 0, 0, 
	2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 3, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0};



/**
  * 
  * This functions cheks if an atom is a built-in
  * predicate.
  * 
  **/
int builtin_check_predicate(Term *term) {
	int key = hashmap_function(BUILTIN_HASH_SIZE, term->term.string);
	return builtin_keys[key] != NULL && wcscmp(term->term.string, builtin_keys[key]) == 0;
}

/**
  * 
  * This functions runs a built-in predicate.
  * 
  **/
int builtin_run_predicate(Program *program, Derivation *D, State *point, Term *term) {
	Term *error, *head;
	int arity, length, key;
	head = term_list_get_head(term);
	key = hashmap_function(BUILTIN_HASH_SIZE, head->term.string);
	if(builtin_keys[key] != NULL && wcscmp(head->term.string, builtin_keys[key]) == 0) {
		arity = builtin_arities[key];
		length = term_list_length(term)-1;
		if(length == -1) {
			error = exception_type_error(L"callable_term", term, term->parent);
			derivation_push_state(D, state_error(point, error));
			term_free(error);
		} else if(length == arity || arity == -1) {
			builtin_handlers[key](program, D, point, term);
		} else {
			error = exception_arity_error(arity, length, term, term->parent);
			derivation_push_state(D, state_error(point, error));
			term_free(error);
		}
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
	Term *path, *error = NULL;
	FILE *file;
	path = term_list_get_nth(term, 1);
	if(term_is_variable(path))
		error = exception_instantiation_error(term->parent);
	else if(!term_is_atom(path) && !term_is_string(path)) 
		error = exception_type_error(L"string_or_atom", path, term->parent);
	if(error != NULL) {
		derivation_push_state(D, state_error(point, error));
		term_free(error);
		return;
	}
	file = fopen(path->term.string, "r");
	if(file != NULL) {
		parser_stream(program, file);
		fclose(file);
		derivation_push_state(D, state_success(point, NULL));
	} else {
		error = exception_existence_error(L"source_sink", path, term->parent);
		derivation_push_state(D, state_error(point, error));
		term_free(error);
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
	Term *path, *error = NULL;
	FILE *file;
	wchar_t *module;
	int size;
	path = term_list_get_nth(term, 1);
	if(term_is_variable(path))
		error = exception_instantiation_error(term->parent);
	else if(!term_is_atom(path) && !term_is_string(path)) 
		error = exception_type_error(L"string_or_atom", path, term->parent);
	if(error != NULL) {
		derivation_push_state(D, state_error(point, error));
		term_free(error);
		return;
	}
	size = wcslen(HERBRAND_PATH)+wcslen(path->term.string)+12;
	module = malloc(sizeof(wchar_t)*size);
	swprintf(module, size, HERBRAND_PATH L"modules/%ls.hb", path->term.string);
	file = fopen(module, "r");
	free(module);
	if(file != NULL) {
		parser_stream(program, file);
		fclose(file);
		derivation_push_state(D, state_success(point, NULL));
	} else {
		error = exception_existence_error(L"module", path, term->parent);
		derivation_push_state(D, state_error(point, error));
		term_free(error);
	}
}

/**
  * 
  * and/*
  * (and ...+callable_term)
  * 
  * Conjunction.
  * (and | Goals) is true if and only if every goal in Goals is true.
  * 
  **/
void builtin_and(Program *program, Derivation *D, State *point, Term *term) {
	Term *tail;
	tail = term_list_get_tail(term);
	derivation_push_state(D, state_success(point, tail));
}

/**
  * 
  * or/*
  * (or ...+callable_term)
  * 
  * Disjunction.
  * (or | Goals) is true if and only if any goal in Goals is true.
  * 
  **/
void builtin_or(Program *program, Derivation *D, State *point, Term *term) {
	Term *list, *head;
	State *state = NULL, *prev;
	list = term_list_get_tail(term);
	while(!term_list_is_null(list)) {
		prev = state;
		state = state_alloc();
		head = term_list_get_head(list);
		state->goal = head;
		state->substitution = NULL;
		state->next = prev;
		term_increase_references(head);
		list = term_list_get_tail(list);
	}
	while(state != NULL) {
		derivation_push_state(D, state_success(point, state->goal));
		prev = state->next;
		state_free(state);
		state = prev;
	}
}

/**
  * 
  * ite/3
  * (ite :callable_term :callable_term :callable_term)
  * 
  * If-Then-Else.
  * (ite If Then Else) is true if and only if If is true and Then is true
  * for the first solution of If, or if If is false and Else is true for
  * the first value with which If fails.
  * 
  **/
void builtin_ite(Program *program, Derivation *D, State *point, Term *term) {
	Term *list, *cut, *list_cut, *cond, *then, *els, *error = NULL;
	cond = term_list_get_nth(term, 1);
	then = term_list_get_nth(term, 2);
	els = term_list_get_nth(term, 3);
	if(term_is_variable(cond) || term_is_variable(then) || term_is_variable(els))
		error = exception_instantiation_error(term->parent);
	else if(!term_is_callable(cond))
		error = exception_type_error(L"callable_term", cond, term->parent);
	else if(!term_is_callable(then))
		error = exception_type_error(L"callable_term", then, term->parent);
	else if(!term_is_callable(els))
		error = exception_type_error(L"callable_term", els, term->parent);
	if(error != NULL) {
		derivation_push_state(D, state_error(point, error));
		term_free(error);
		return;
	}
	// Cut term
	cut = term_alloc();
	cut->type = TYPE_ATOM;
	term_set_string(cut, L"!");
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

/**
  * 
  * !/0
  * (!)
  * 
  * Cut.
  * (!) is true. All choice points between the cut and the parent goal are
  * removed. The effect is commit to use of both the current clause and
  * the substitutions found at the point of the cut.
  * 
  **/
void builtin_cut(Program *program, Derivation *D, State *point, Term *term) {
	int i;
	Term *left, *cut;
	State *parent_cut, *last_cut, *state, *next_state;
	parent_cut = point->parent;
	cut = term_list_get_nth(term, 0);
	while(parent_cut != NULL && term_search_term(parent_cut->goal, cut)) {
		last_cut = parent_cut;
		parent_cut = parent_cut->parent;
		if(parent_cut != NULL && parent_cut->goal != NULL && !term_list_is_null(parent_cut->goal)) {
			left = term_select_most_left(parent_cut->goal);
			if(left != NULL
			&& wcscmp(term_list_get_head(left)->term.string, L"call") == 0
			&& term_search_term(left, cut)) {
				parent_cut = last_cut;
				break;
			}
		}
	}
	state = D->points;
	while(1) {
		if(state == NULL)
			break;
		next_state = state;
		while(next_state != NULL && next_state != parent_cut)
			next_state = next_state->parent;
		if(next_state == parent_cut) {
			D->nb_states--;
			next_state = state->next;
			state_free(state);
			state = next_state;
		} else {
			break;
		}
	}
	D->points = state;
	derivation_push_state(D, state_success(point, NULL));
}

/**
  * 
  * not/1
  * (not @callable_term)
  * 
  * Not provable.
  * (not Term) is true if and only if (call Term) is false.
  * 
  **/
void builtin_not(Program *program, Derivation *D, State *point, Term *term) {
	Term *list, *cut, *call, *fail, *list_cut, *list_fail, *goal, *list_goal;
	goal = term_list_get_nth(term, 1);
	// Cut term
	cut = term_alloc();
	cut->type = TYPE_ATOM;
	term_set_string(cut, L"!");
	// Cut call
	list_cut = term_list_empty();
	term_list_add_element(list_cut, cut);
	// False term
	fail = term_alloc();
	fail->type = TYPE_ATOM;
	term_set_string(fail, L"false");
	// False call
	list_fail = term_list_empty();
	term_list_add_element(list_fail, fail);
	// Call term
	call = term_alloc();
	call->type = TYPE_ATOM;
	term_set_string(call, L"call");
	// Goal call
	list_goal = term_list_empty();
	term_list_add_element(list_goal, call);
	term_list_add_element(list_goal, goal);
	term_increase_references(goal);
	// ((call Goal) (!) (false))
	list = term_list_empty();
	term_list_add_element(list, list_goal);
	term_list_add_element(list, list_cut);
	term_list_add_element(list, list_fail);
	derivation_push_state(D, state_success(point, NULL));
	derivation_push_state(D, state_success(point, list));
	term_free(list);
}

/**
  * 
  * call/*
  * (call +callable_term ...+term)
  * 
  * Invoke a callable term as a goal.
  * (call Goal | Args) is true if and only if Goal represents a goal which is
  * true after appending Args to its list of arguments. 
  * 
  **/
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

/**
  * 
  * once/1
  * (once @callable_term)
  * 
  * Evaluate a term just once.
  * (once Term) is true. once makes sure that Term fails or succeeds just once.
  * 
  **/
void builtin_once(Program *program, Derivation *D, State *point, Term *term) {
	Term *list, *cut, *call, *list_cut, *goal, *list_goal;
	goal = term_list_get_nth(term, 1);
	// Cut term
	cut = term_alloc();
	cut->type = TYPE_ATOM;
	term_set_string(cut, L"!");
	// Call term
	call = term_alloc();
	call->type = TYPE_ATOM;
	term_set_string(call, L"call");
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
  * (true) is always true.
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
  * (false) is always false.
  * 
  **/
void builtin_false(Program *program, Derivation *D, State *point, Term *term) {
	// do nothing
	// fail
}

/**
  * 
  * catch/3
  * (catch +callable_term ?term +callable_term)
  * 
  * Enable recovery from exceptions.
  * (catch Goal Catcher Handler) behaves as call/1 if no exception is raised when
  * executing Goal. If an exception is raised using throw/1 while Goal executes,
  * and the Goal is the innermost goal for which Catcher unifies with the argument
  * of throw/1, all choice points generated by Goal are cut, the system backtracks
  * to the start of catch/3 while preserving the thrown exception term, and Handler
  * is called as in call/1.
  * 
  **/
void builtin_catch(Program *program, Derivation *D, State *point, Term *term) {
	Term *goal, *call, *list_call, *list, *control, *list_control;
	goal = term_list_get_nth(term, 1);
	// Call term
	call = term_alloc();
	call->type = TYPE_ATOM;
	term_set_string(call, L"call");
	// True term
	control = term_alloc();
	control->type = TYPE_ATOM;
	term_set_string(control, L"$catcher");
	control->parent = point;
	// Goal call
	list_call = term_list_empty();
	term_list_add_element(list_call, call);
	term_list_add_element(list_call, goal);
	term_increase_references(goal);
	// True call
	list_control = term_list_empty();
	term_list_add_element(list_control, control);
	// ((call Goal) (true))
	list = term_list_empty();
	term_list_add_element(list, list_call);
	term_list_add_element(list, list_control);
	derivation_push_state(D, state_success(point, list));
	term_free(list);
}

/**
  * 
  * throw/1
  * (throw +term)
  * 
  * Raise an exception.
  * (throw Exception) raise the Exception exception. The system looks for the innermost
  * catch/3 ancestor for which Exception unifies with the Catcher argument of the catch/3
  * call.
  * 
  **/
void builtin_throw(Program *program, Derivation *D, State *point, Term *term) {
	Term *error;
	error = term_list_get_nth(term, 1);
	derivation_push_state(D, state_error(point, error));
}

/**
  * 
  * $catcher/0
  * ($catcher)
  * 
  * Alwais succeed.
  * ($catcher) is always true.
  * 
  **/
void builtin__catcher(Program *program, Derivation *D, State *point, Term *term) {
	derivation_push_state(D, state_success(point, NULL));
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
void builtin_not_unification(Program *program, Derivation *D, State *point, Term *term) {
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
	Term *error = NULL, *message = term_list_get_nth(term, 1);
	if(term_is_variable(message))
		error = exception_instantiation_error(term->parent);
	else if(!term_is_integer(message)) 
		error = exception_type_error(L"integer", message, term->parent);
	if(error != NULL) {
		derivation_push_state(D, state_error(point, error));
		term_free(error);
		return;
	}
	exit(message->term.numeral);
}

void builtin_current_herbrand_flag(Program *program, Derivation *D, State *point, Term *term) {
}
void builtin_set_herbrand_flag(Program *program, Derivation *D, State *point, Term *term) {
}
