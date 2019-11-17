/*H*
 * 
 * FILENAME: semantics.c
 * DESCRIPTION: Declarative semantics for the language
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 06.04.2019
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
	State *state;
	Term *error, *callable;
	if(term_is_callable(goal))
		state = state_init_goal(goal);
	else {
		callable = term_init_atom(L"callable");
		error = exception_type_error(callable, goal, NULL);
		state = state_error(NULL, error);
		term_free(error);
		term_free(callable);
	}
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
	parent_catch = point->next;
	error = substitution_get_link(point->substitution, L"$error");
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
		while(parent->next != NULL && parent->next != parent_catch)
			parent = parent->next;
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
	Term *term, *body, *error, *head, *type, *callable;
	Substitution *mgu, *subs;
	wchar_t *name;
	while(1) {
		point = D->points;
		// If no more points, there are no more answers
		if(point == NULL)
			return NULL;
		if(point->most_left == NULL)
			point->most_left = term_select_most_left(point->goal);
		term = point->most_left;
		// If there is an error, return it
		if(substitution_get_link(point->substitution, L"$error") != NULL) {
			derivation_pop_state(D);
			// Catch exception
			if(semantics_catch(D, point))
				continue;
			mgu = point->substitution;
			substitution_increase_references(mgu);
			state_free(point);
			while(D->points != NULL)
				state_free(derivation_pop_state(D));
			return mgu;
		}
		// If no more terms, this choice point is an answer
		if(term == NULL || term_list_is_null(term)) {
			// Remove point
			derivation_pop_state(D);
			mgu = point->substitution;
			substitution_increase_references(mgu);
			state_free(point);
			return mgu;
		}
		// Else, do a resolution step

		// If not callable term, error
		if(!term_is_callable(term)) {
			derivation_pop_state(D);
			callable = term_init_atom(L"callable");
			error = exception_type_error(callable, term, term->parent);
			derivation_push_state(D, state_error(point, error));
			state_free(point);
			term_free(error);
			term_free(callable);
		// If is a built-in predicate
		} else if(builtin_check_predicate(term_list_get_head(term))) {
			// Remove point
			derivation_pop_state(D);
			// Run built-in predicate
			builtin_run_predicate(program, D, point, term);
			// Free point
			name = term_list_get_head(term)->term.string;
			if(wcscmp(name, L"catch") != 0 && wcscmp(name, L"call"))
				state_free(point);
			else
				derivation_push_visited_state(D, point);
		// User or module predicate
		} else {
			rule = program_get_predicate(program, term->term.list->head->term.string, term->parent);
			// If rule does not exist, fail
			if(rule == NULL) {
				derivation_pop_state(D);
				state_free(point);
				continue;
			}
			// Check arity
			length = term_list_length(term)-1;
			if(rule->arity != length) {
				error = exception_arity_error(rule->arity, length, term, term->parent);
				derivation_push_state(D, state_error(point, error));
				term_free(error);
				continue;
			}
			// Check type
			type = rule->type;
			subs = tc_check_type_expr(term, type);
			if(subs == NULL) {
				error = exception_type_error(type, term, term->parent);
				derivation_push_state(D, state_error(point, error));
				term_free(error);
				continue;
			} else {
				substitution_free(subs);
			}
			// Check tail recursion
			if(term->parent != NULL && rule->tail_recursive && wcscmp(term_list_get_head(term)->term.string, term->parent) == 0) {
				state = D->points->next;
				while(state != NULL && (state->most_left == NULL
				|| wcscmp(term_list_get_head(state->most_left)->term.string, term->parent) != 0)) {
					D->points = state->next;
					state_free(state);
					state = D->points;
				}
				if(state != NULL) {
					D->points = state->next;
					state_free(state);
					state = D->points;
				}
				point->next = state;
				D->points = point;
			}
			if(++point->rule_inference >= rule->nb_clauses) {
				// Remove point
				derivation_pop_state(D);
				state_free(point);
			} else {
				for(i = point->rule_inference; i < rule->nb_clauses; i++) {
					// Check unification with next rule
					clause = clause_rename_variables(rule->clauses[i], &(program->renames));
					mgu = semantics_unify_terms(term->term.list->tail, clause->head, 0);
					if(mgu != NULL) {
						state = state_inference(point, clause->body, mgu);
						substitution_free(mgu);
						derivation_push_state(D, state);
						clause_free(clause);
						break;
					}
					clause_free(clause);
				}
				point->rule_inference = i;
			}
		}
	}
}