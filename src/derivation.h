/*H*
 * 
 * FILENAME: derivation.h
 * DESCRIPTION: Data structures and functions for derivations
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 30.03.2019
 * 
 *H*/

#include "term.h"
#include "substitution.h"



#ifndef LOGIC_DERIVATION_H
#define LOGIC_DERIVATION_H

typedef struct State {
  Term *goal;
  Substitution *substitution;
	struct State *parent;
  struct State *next;
} State;

typedef struct Derivation {
	State *points;
  State *visited;
  int nb_states;
  int nb_visited_states;
  int nb_inferences;
} Derivation;

#endif



/**
  * 
  * This function creates a derivaiton returning a pointer
  * to a newly initialized Derivation struct.
  * 
  **/
Derivation *derivation_alloc();

/**
  * 
  * This function frees a previously allocated derivation.
  * The states and terms underlying the derivation will
  * also be deallocated.
  * 
  **/
void derivation_free(Derivation *D);

/**
  * 
  * This function pushes a new state at the beginning
  * of a derivation.
  * 
  **/
void derivation_push_state(Derivation *D, State *state);

/**
  * 
  * This function pushes an old state at the beginning
  * of visited states.
  * 
  **/
void derivation_push_visited_state(Derivation *D, State *state);

/**
  * 
  * This function pops a new state from the beginning
  * of a derivation.
  * 
  **/
State *derivation_pop_state(Derivation *D);

/**
  * 
  * This function creates an state returning a pointer
  * to a newly initialized State struct.
  * 
  **/
State *state_alloc();

/**
  * 
  * This function frees a previously allocated state.
  * The terms and substitution underlying the state will
  * also be deallocated.
  * 
  **/
void state_free(State *state);

/**
  *
  * This function creates an state from an inference,
  * returning a pointer to a newly initialized State
  * struct.
  * 
  **/
State *state_inference(State *point, Term *body, Substitution *subs);

/**
  *
  * This function creates an state from an success step,
  * returning a pointer to a newly initialized State
  * struct.
  * 
  **/
State *state_success(State *point);

/**
  * 
  * This function creates an state from a new goal,
  * returning a pointer to a newly initialized State
  * struct.
  * 
  **/
State *state_init_goal(Term *goal);

/**
  * 
  * This function prints for the standard output
  * the whole state.
  * 
  **/
void state_print(State *state);