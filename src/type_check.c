/*H*
 * 
 * FILENAME: clause.c
 * DESCRIPTION: Data structures and functions for type checking
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 14.11.2019
 * 
 *H*/

#include "type_check.h"

/**
  * 
  * This function returns a pointer to a term
  * containing the type of a given term.
  * 
  **/
Term *tc_get_type_term(Term *term) {
    Term *type_list, *type_head;
    Substitution *subs;
    if(term_is_atom(term))
        return term_init_atom(L"atom");
    else if(term_is_float(term))
        return term_init_atom(L"float");
    else if(term_is_integer(term))
        return term_init_atom(L"int");
    else if(term_is_variable(term))
        return term_init_atom(L"var");
    else if(term_is_char(term))
        return term_init_atom(L"char");
    else if(term_is_string(term)) {
        type_list = term_list_empty();
        term_list_add_element(type_list, term_init_atom(L"list"));
        term_list_add_element(type_list, term_init_atom(L"char"));
        return type_list;
    } else if(term_is_list(term)) {
        type_list = NULL;
        while(!term_list_is_null(term)) {
            type_head = tc_get_type_term(term_list_get_head(term));
            term = term_list_get_tail(term);
            if(type_list == NULL) {
                type_list = type_head;
            } else {
                subs = semantics_unify_terms(type_list, type_head, 1);
                term_free(type_head);
                if(subs != NULL) {
                    type_head = type_list;
                    type_list = term_apply_substitution(type_list, subs);
                    term_free(type_head);
                    substitution_free(subs);
                } else {
                    term_free(type_list);
                    type_list = term_list_empty();
                    type_head = term_list_add_element(type_list, term_init_atom(L"list"));
                    term_list_add_element(type_head, term_init_atom(L"term"));
                    return type_list;
                }
            }
        }
        type_head = term_list_empty();
        term_list_add_element(type_head, term_init_atom(L"list"));
        if(type_list == NULL)
            term_list_add_element(type_head, term_init_atom(L"term"));
        else
            term_list_add_element(type_head, type_list);
        return type_head;
    }
}

/**
  * 
  * This function returns a pointer to a term
  * containing the type of a given callable
  * expression.
  * 
  **/
Term *tc_get_type_expr(Term *expr) {

}

/**
  * 
  * This function checks the type of an expression,
  * returning a pointer to a substitution if the
  * expression type checks. Otherwise, returns null.
  * 
  **/
Substitution *tc_check_type_expr(Term *expr, Term *type) {
    Term *type_expr;
    Substitution *subs;
    type_expr = tc_get_type_expr(expr);
    subs = semantics_unify_terms(type, type_expr, 1);
    term_free(type_expr);
    return subs;
}