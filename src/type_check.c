/*H*
 * 
 * FILENAME: clause.c
 * DESCRIPTION: Data structures and functions for type checking
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 16.11.2019
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
        while(term_is_list(term) && !term_list_is_null(term)) {
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
        if(!term_is_list(term) && !term_is_variable(term)) {
            if(type_list == NULL || !term_is_atom(type_list)
            || wcscmp(L"char", type_list->term.string) != 0 || !term_is_string(term)) {
                term_free(type_list);
                return term_init_atom(L"term");
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
    Term *type, *head;
    if(!term_is_list(expr) || term_list_is_null(expr))
        return tc_get_type_term(expr);
    expr = term_list_get_tail(expr);
    type = term_list_empty();
    while(term_is_list(expr) && !term_list_is_null(expr)) {
        head = tc_get_type_term(term_list_get_head(expr));
        expr = term_list_get_tail(expr);
        term_list_add_element(type, head);
    }
    if(!term_is_list(expr)) {
        term_free(type);
        return term_init_atom(L"term");
    }
    return type;
}

/**
  * 
  * This function returns a pointer to a term
  * containing normlized version of the type
  * 
  * callable => (list _)
  * string => (list char)
  * term => _ (anonymous variable)
  * var => _ (anonymous variable)
  * int => (num numeral)
  * float => (num decimal)
  * number => (num _)
  * 
  **/
Term *tc_normalize_type(Term *type) {
    Term *list;
    if(term_is_atom(type)) {
        if(wcscmp(L"term", type->term.string) == 0) {
            return term_init_variable(L"_");
        } else if(wcscmp(L"var", type->term.string) == 0) {
            return term_init_variable(L"_");
        } else if(wcscmp(L"string", type->term.string) == 0) {
            list = term_list_empty();
            term_list_add_element(list, term_init_atom(L"list"));
            term_list_add_element(list, term_init_atom(L"char"));
            return list;
        } else if(wcscmp(L"callable", type->term.string) == 0) {
            list = term_list_empty();
            term_list_add_element(list, term_init_atom(L"list"));
            term_list_add_element(list, term_init_variable(L"_"));
            return list;
        } else if(wcscmp(L"int", type->term.string) == 0) {
            list = term_list_empty();
            term_list_add_element(list, term_init_atom(L"num"));
            term_list_add_element(list, term_init_atom(L"numeral"));
            return list;
        } else if(wcscmp(L"float", type->term.string) == 0) {
            list = term_list_empty();
            term_list_add_element(list, term_init_atom(L"num"));
            term_list_add_element(list, term_init_atom(L"decimal"));
            return list;
        } else if(wcscmp(L"number", type->term.string) == 0) {
            list = term_list_empty();
            term_list_add_element(list, term_init_atom(L"num"));
            term_list_add_element(list, term_init_variable(L"_"));
            return list;
        }
    } else if(term_is_list(type)) {
        list = term_list_empty();
        while(!term_list_is_null(type)) {
            term_list_add_element(list, tc_normalize_type(term_list_get_head(type)));
            type = term_list_get_tail(type);
        }
        return list;
    }
    term_increase_references(type);
    return type;
}

/**
  * 
  * This function checks the type of an expression,
  * returning a pointer to a substitution if the
  * expression type checks. Otherwise, returns null.
  * 
  **/
Substitution *tc_check_type_expr(Term *expr, Term *type) {
    Term *type_expr, *norm_type, *norm_type_expr;
    Substitution *subs;
    type_expr = tc_get_type_expr(expr);
    norm_type_expr = tc_normalize_type(type_expr);
    norm_type = tc_normalize_type(type);
    subs = semantics_unify_terms(norm_type, norm_type_expr, 1);
    term_free(type_expr);
    term_free(norm_type_expr);
    term_free(norm_type);
    return subs;
}