/*H*
 * 
 * FILENAME: semantics.c
 * DESCRIPTION: Declarative semantics for the language
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 05.04.2019
 * 
 *H*/

#include "semantics.h"



/**
  * 
  * This function creates a derivation from a new goal,
  * returning a pointer to a newly initialized Derivation
  * struct.
  *
  **/
Derivation *semantics_query(Term *goal) {
	Derivation *D = derivation_alloc();
	State *state = state_init_goal(goal);
	derivation_push_state(D, state);
	return D;
}

/**
  * 
  * This function tries to catch an exception. Returns
  * 1 when the exception is catched, or 0 otherwise.
  *
  **/
int semantics_catch(Derivation *D, State *point) {
	int i, nb_catchers = 0;
	void **catchers;
	State *parent_catch, *state, *parent;
	Term *left, *catcher, *handler, *error;
	Substitution *mgu = NULL;
	parent_catch = point->parent;
	error = substitution_get_link(point->substitution, "$error");
	// Find catcher
	if(parent_catch == NULL)
		return 0;
	catchers = term_get_catchers(parent_catch->goal, &nb_catchers);
	for(i = 0; i < nb_catchers && mgu == NULL; i++) {
		parent_catch = (State*)catchers[i];
		left = term_select_most_left(parent_catch->goal);
		catcher = term_list_get_nth(left, 2);
		mgu = semantics_unify_terms(catcher, error, 0);
	}
	free(catchers);
	if(mgu == NULL)
		return 0;
	// Remove choice points from original catcher
	while(D->points != NULL) {
		state = D->points;
		parent = state;
		while(state->parent != NULL && state->parent != parent_catch)
			parent = parent->parent;
		if(parent == NULL)
			break;
		D->points = state->next;
		D->nb_states--;
		state_free(state);
	}
	// Add new point
	handler = term_list_get_nth(left, 3);
	state = state_inference(parent_catch, handler, mgu);
	derivation_push_state(D, state);
	substitution_free(mgu);
	return 1;
}

/**
  * 
  * This function finds and returns the next computed
  * answer of a derivation.
  *
  **/
Substitution *semantics_answer(Program *program, Derivation *D) {
	int i, length;
	State *point, *state;
	Rule *rule;
	Clause *clause;
	Term *term, *body, *error;
	Substitution *mgu;
	while(1) {
		point = derivation_pop_state(D);
		// If no more points, there are no more answers
		if(point == NULL)
			return NULL;
		derivation_push_visited_state(D, point);
		term = term_select_most_left(point->goal);
		// If there is an error, return it
		if(substitution_get_link(point->substitution, "$error") != NULL) {
			// Catch exception
			if(semantics_catch(D, point))
				continue;
			// Return exception
			if(term != NULL)
				term_free(term);
			return point->substitution;
		}
		// If no more terms, this choice point is an answer
		if(term == NULL || term_list_is_null(term)) {
			if(term != NULL)
				term_free(term);
			return point->substitution;
		}
		// Else, do a resolution step

		// If not callable term, error
		if(!term_is_callable(term)) {
			error = exception_type_error("callable_term", term, term->parent);
			derivation_push_state(D, state_error(point, error));
			term_free(error);
		// If is a built-in predicate
		} else if(builtin_check_predicate(term_list_get_head(term))) {
			// Run built-in predicate
			builtin_run_predicate(program, D, point, term);
		// User or module predicate
		} else {
			rule = program_get_predicate(program, term->term.list->head->term.string, term->parent);
			// If rule does not exist, fail
			if(rule == NULL)
				continue;
			// Check arity
			length = term_list_length(term)-1;
			if(rule->arity != length) {
				error = exception_arity_error(rule->arity, length, term, term->parent);
				derivation_push_state(D, state_error(point, error));
				term_free(error);
				continue;
			}
			// For each clause in the rule, check unification
			for(i = rule->nb_clauses-1; i >= 0; i--) {
				clause = clause_rename_variables(rule->clauses[i], &(program->renames));
				mgu = semantics_unify_terms(term->term.list->tail, clause->head, 0);
				if(mgu != NULL) {
					state = state_inference(point, clause->body, mgu);
					derivation_push_state(D, state);
					substitution_free(mgu);
				}
				clause_free(clause);
			}
		}
	}
}