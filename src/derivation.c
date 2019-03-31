/*H*
 * 
 * FILENAME: derivation.c
 * DESCRIPTION: Data structures and functions for derivations
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 31.03.2019
 * 
 *H*/

#include "derivation.h"



/**
  * 
  * This function creates a derivaiton returning a pointer
  * to a newly initialized Derivation struct.
  * 
  **/
Derivation *derivation_alloc() {
	Derivation *D = malloc(sizeof(Derivation));
	D->points = NULL;
	D->visited = NULL;
	D->nb_states = 0;
	D->nb_visited_states = 0;
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
	State *next, *point = D->points;
	while(point != NULL) {
		next = point->next;
		state_free(point);
		point = next;
	}
	point = D->visited;
	while(point != NULL) {
		next = point->next;
		state_free(point);
		point = next;
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
  * This function pushes an old state at the beginning
  * of visited states.
  * 
  **/
void derivation_push_visited_state(Derivation *D, State *state) {
	state->next = D->visited;
	D->visited = state;
	D->nb_visited_states++;
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
  * This function frees a previously allocated state.
  * The terms and substitution underlying the state will
  * also be deallocated.
  * 
  **/
void state_free(State *state) {
	term_free(state->goal);
	substitution_free(state->substitution);
	free(state);
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
	state->substitution = substitution_alloc_from_term(goal);
	term_increase_references(goal);
	return state;
}

/**
  *
  * This function creates an state from a inference,
  * returning a pointer to a newly initialized State
  * struct.
  * 
  **/
State *state_inference(State *point, Term *body, Substitution *subs) {
	Term *left;
	State *state = state_alloc();
	left = term_replace_most_left(point->goal, body);
	state->goal = term_apply_substitution(left, subs);
  	if(left != NULL)
		term_free(left);
	state->substitution = substitution_compose(point->substitution, subs, 0);
	state->parent = point;
	return state;
}

/**
  * 
  * This function prints for the standard output
  * the whole state.
  * 
  **/
void state_print(State *state) {
	printf("<");
	term_print(state->goal);
	printf(", ");
	substitution_print(state->substitution);
	printf(">");
}