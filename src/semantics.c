/*H*
 * 
 * FILENAME: semantics.c
 * DESCRIPTION: Declarative semantics for the language
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 28.03.2019
 * 
 *H*/

#include "semantics.h"



// UNIFICATION

/**
  * 
  * This function returns the most general unifier of two terms.
  * If terms are not unifiable, returns NULL.
  * 
  **/
Substitution *semantics_unify_terms(Term *term1, Term *term2, int occurs_check) {
	Substitution *subs;
	if(
		term1->type == TYPE_VARIABLE && strcmp(term1->term.string, "_") == 0 ||
		term2->type == TYPE_VARIABLE && strcmp(term2->term.string, "_") == 0
	) {
		return substitution_alloc(0);
	} else if(term1->type == TYPE_VARIABLE) {
		subs = substitution_alloc(1);
		substitution_add_link(subs, term1->term.string, term2);
		return subs;
	} else if(term2->type == TYPE_VARIABLE) {
		subs = substitution_alloc(1);
		substitution_add_link(subs, term2->term.string, term1);
		return subs;
	} else if(term1->type == term2->type) {
		switch(term1->type) {
			case TYPE_LIST:
				return semantics_unify_lists(term1, term2, occurs_check); 
			case TYPE_ATOM:
				return semantics_unify_atoms(term1, term2);
			case TYPE_STRING:
				return semantics_unify_strings(term1, term2);
			case TYPE_NUMERAL:
				return semantics_unify_numerals(term1, term2);
			case TYPE_DECIMAL:
				return semantics_unify_decimals(term1, term2);
			default:
				return NULL;
		}
	}
	return NULL;
}

/**
  * 
  * This function returns the most general unifier of two lists.
  * If lists are not unifiable, returns NULL.
  * 
  **/
Substitution *semantics_unify_lists(Term *term1, Term *term2, int occurs_check) {
	Term *list1, *list2;
	Substitution *mgu, *subs2, *subs = substitution_alloc(0);
	if(term_list_is_null(term1) && term_list_is_null(term2))
		return subs;
	else if(term_list_is_null(term1) || term_list_is_null(term2))
		return NULL;
	while(term1->type == TYPE_LIST && term2->type == TYPE_LIST
	&& !term_list_is_null(term1) && !term_list_is_null(term2)) {
		mgu = semantics_unify_terms(
			term_list_get_head(term1),
			term_list_get_head(term2),
			occurs_check);
		if(mgu == NULL)
			return NULL;
		subs2 = substitution_compose(subs, mgu);
		term1 = term_apply_substitution(term_list_get_tail(term1), mgu);
		term2 = term_apply_substitution(term_list_get_tail(term2), mgu);
		substitution_free(subs);
		substitution_free(mgu);
		subs = subs2;
	}
	mgu = semantics_unify_terms(term1, term2, occurs_check);
	if(mgu == NULL)
		return NULL;
	subs2 = substitution_compose(subs, mgu);
	substitution_free(subs);
	substitution_free(mgu);
	return subs2;
}

/**
  * 
  * This function returns the most general unifier of two numerals.
  * If numerals are not unifiable, returns NULL.
  * 
  **/
Substitution *semantics_unify_numerals(Term *num1, Term *num2) {
	if(num1->term.numeral ==  num2->term.numeral)
		return substitution_alloc(0); 
	return NULL;
}

/**
  * 
  * This function returns the most general unifier of two decimals.
  * If decimals are not unifiable, returns NULL.
  * 
  **/
Substitution *semantics_unify_decimals(Term *dec1, Term *dec2) {
	if(dec1->term.decimal == dec2->term.decimal)
		return substitution_alloc(0); 
	return NULL;
}

/**
  * 
  * This function returns the most general unifier of two atoms.
  * If atoms are not unifiable, returns NULL.
  * 
  **/
Substitution *semantics_unify_atoms(Term *atom1, Term *atom2) {
	if(strcmp(atom1->term.string, atom2->term.string) == 0)
		return substitution_alloc(0); 
	return NULL;
}

/**
  * 
  * This function returns the most general unifier of two strings.
  * If strings are not unifiable, returns NULL.
  * 
  **/
Substitution *semantics_unify_strings(Term *str1, Term *str2) {
	if(strcmp(str1->term.string, str2->term.string) == 0)
		return substitution_alloc(0); 
	return NULL;
}



// RESOLUTION

/**
  * 
  * This function creates a derivaiton returning a pointer
  * to a newly initialized Derivation struct.
  * 
  **/
Derivation *derivation_alloc() {
	Derivation *D = malloc(sizeof(Derivation));
	D->points = NULL;
	D->nb_states = 0;
	D->nb_inferences = 0;
	return D;
};

/**
  * 
  * This function frees a previously allocated derivation.
  * The states and terms underlying the derivation will
  * also be deallocated.
  * 
  **/
void derivation_free(Derivation *D) {
	State *point = D->points;
	while(point != NULL) {
		point = D->points->next;
		state_free(D->points);
	}
	free(D);
}

/**
  * 
  * This function pushes a new state at the beginning
  * of a derivation.
  * 
  **/
void derivation_push_state(Derivation *D, State *state) {
	state->next = D->points;
	D->points = state;
	D->nb_states++;
}

/**
  * 
  * This function pops a new state from the beginning
  * of a derivation.
  * 
  **/
State *derivation_pop_state(Derivation *D) {
	State *state = D->points;
	if(state != NULL)
		D->points = state->next;
	D->nb_states--;
	return state;
}

/**
  * 
  * This function creates an state returning a pointer
  * to a newly initialized State struct.
  * 
  **/
State *state_alloc() {
	State *state = malloc(sizeof(State));
	state->goal = NULL;
	state->next = NULL;
	state->parent = NULL;
	state->substitution = NULL;
	return state;
}

/**
  * 
  * This function creates an state from a new goal,
  * returning a pointer to a newly initialized State
  * struct.
  * 
  **/
State *state_init_goal(Term *goal) {
	State *state = state_alloc();
	state->goal = goal;
	//state->substitution = substitution_alloc_from_term(goal);
	state->substitution = substitution_alloc(0);
	return state;
}

/**
  * 
  * This function selects the most left term
  * of the goal in a state.
  * 
  **/
Term *term_select_most_left(Term *term) {
	if(term == NULL || term_list_is_null(term))
		return NULL;
	while(term->type == TYPE_LIST && term->term.list->head->type == TYPE_LIST)
		term = term->term.list->head;
	return term;
}

/**
  * 
  * This function replaces the most left term
  * of the goal in a state.
  * 
  **/
Term *term_replace_most_left(Term *term, Term *head) {
	if(term == NULL || term_list_is_null(term))
		return head;
	if(term->type == TYPE_LIST) {
		if(term->term.list->head->type == TYPE_LIST) {
			return term_list_create(
				term_replace_most_left(term->term.list->head, head),
				term->term.list->tail);
		} else {
			if(head == NULL) {
				return term->term.list->tail;
			} else {
				return term_list_create(head, term->term.list->tail);
			}
		}
	}
	return NULL;
}

/**
  * 
  * This function frees a previously allocated state.
  * The terms and substitution underlying the state will
  * also be deallocated.
  * 
  **/
void state_free(State *state) {
	term_free(state->goal);
	substitution_free(state->substitution);
	if(state->parent != NULL)
		state_free(state->parent);
	free(state);
}

/**
  *
  * This function creates an state from a inference,
  * returning a pointer to a newly initialized State
  * struct.
  * 
  **/
State *state_inference(State *point, Term *body, Substitution *subs) {
	State *state = state_alloc();
	state->goal = term_apply_substitution(term_replace_most_left(state->goal, body), subs);
	state->substitution = substitution_compose(point->substitution, subs);
	state->parent = point;
	return state;
}

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
  * This function finds and returns the next computed
  * answer of a derivation.
  *
  **/
Substitution *semantics_answer(Program *program, Derivation *D) {
	int i;
	State *point, *state;
	Rule *rule;
	Clause *clause;
	Term *term, *body;
	Substitution *mgu;
	while(1) {
		point = derivation_pop_state(D);
		// If no more points, there are no more answers
		if(point == NULL)
			return NULL;
		term = term_select_most_left(point->goal);
		// If no more terms, this choice point is an answer
		if(term == NULL)
			return point->substitution;
		// Else, do a resolution step
		// If not callable, error
		//if(!term_is_callable(term))
			// error
		rule = program_get_rule(program, term->term.list->head->term.string);
		// If rule does not exist, fail
		if(rule == NULL)
			continue;
		// For each clause in the rule, check unification
		for(i = rule->nb_clauses-1; i >= 0; i--) {
			clause = clause_rename_variables(rule->clauses[i]);
			mgu = semantics_unify_terms(clause->head, term->term.list->tail, 0);
			if(mgu != NULL) {
				state = state_inference(point, clause->body, mgu);
				derivation_push_state(D, state);
			}
		}
	}
}