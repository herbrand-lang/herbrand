/*H*
 * 
 * FILENAME: semantics.c
 * DESCRIPTION: Declarative semantics for the language
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 27.03.2019
 * 
 *H*/

#include "semantics.h"



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
		return &LOGIC_SUBSTITUTION_ID;
	} else if(term1->type == TYPE_VARIABLE) {
		subs = substitution_alloc(1);
		substitution_add_link(subs, term1, term2);
		return subs;
	} else if(term2->type == TYPE_VARIABLE) {
		subs = substitution_alloc(1);
		substitution_add_link(subs, term2, term1);
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
	Substitution *subs = &LOGIC_SUBSTITUTION_ID, *mgu;
	if(term_list_is_null(list1) && term_list_is_null(term2))
		return subs;
	while(term1->type == TYPE_LIST && term2->type == TYPE_LIST
	&& !term_list_is_null(list1) && !term_list_is_null(term2)) {
		mgu = semantics_unify_terms(
			term_list_get_head(term1),
			term_list_get_head(term2),
			occurs_check);
		if(mgu == NULL)
			return NULL;
		substitution_compose(subs, mgu);
		term1 = term_list_get_tail(term1);
		term2 = term_list_get_tail(term2);
	}
	mgu = semantics_unify_terms(term1, term2, occurs_check);
	if(mgu == NULL)
		return NULL;
	substitution_compose(subs, mgu);
	return subs;
}

/**
  * 
  * This function returns the most general unifier of two numerals.
  * If numerals are not unifiable, returns NULL.
  * 
  **/
Substitution *semantics_unify_numerals(Term *num1, Term *num2) {
	if(num1->term.numeral ==  num2->term.numeral)
		return &LOGIC_SUBSTITUTION_ID; 
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
		return &LOGIC_SUBSTITUTION_ID; 
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
		return &LOGIC_SUBSTITUTION_ID; 
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
		return &LOGIC_SUBSTITUTION_ID; 
	return NULL;
}