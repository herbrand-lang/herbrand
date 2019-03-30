/*H*
 * 
 * FILENAME: builtin.c
 * DESCRIPTION: Functions for evaluating built-in predicates
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 30.03.2019
 * 
 *H*/

#include "builtin.h"



void *builtin_array[1] = {
    builtin_unification_2
};



/**
  * 
  * This functions cheks if an atom is a built-in
  * predicate.
  * 
  **/
int builtin_check_predicate(Term *term) {
    return term->type == TYPE_ATOM &&
        strcmp("=", term->term.string) == 0;
}

/**
  * 
  * This functions runs a built-in predicate.
  * 
  **/
int builtin_run_predicate(Program *program, Derivation *D, State *point, Term *term) {
    builtin_unification_2(program, D, point, term);
}

/**
  * 
  * Syntactic unification (=/2)
  * 
  **/
void builtin_unification_2(Program *program, Derivation *D, State *point, Term *term) {
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