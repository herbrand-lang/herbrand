/*H*
 * 
 * FILENAME: type_check.c
 * DESCRIPTION: Data structures and functions for type checking
 * AUTHORS: José Antonio Riaza Valverde
 * UPDATED: 18.11.2019
 * 
 *H*/

#include "type_check.h"

/**
  * 
  * This function returns a pointer to a term
  * containing the unification of two types.
  * 
  **/
Term *tc_join_types(Term *type1, Term *type2) {
    Term *type, *head;
    if(term_is_atom(type1) && term_is_atom(type2)) {
        if(wcscmp(type1->term.string, type2->term.string) == 0) {
            term_increase_references(type1);
            return type1;
        } else if(wcscmp(type1->term.string, L"var") == 0) {
            term_increase_references(type2);
            return type2;
        } else if(wcscmp(type2->term.string, L"var") == 0) {
            term_increase_references(type1);
            return type1;
        } else if(wcscmp(type1->term.string, L"int") == 0 && (
            wcscmp(type2->term.string, L"float") == 0 ||
            wcscmp(type2->term.string, L"number") == 0)
        ) {
            return term_init_atom(L"number");
        } else if(wcscmp(type1->term.string, L"float") == 0 && (
            wcscmp(type2->term.string, L"int") == 0 ||
            wcscmp(type2->term.string, L"number") == 0)
        ) {
            return term_init_atom(L"number");
        } else if(wcscmp(type2->term.string, L"int") == 0 && (
            wcscmp(type1->term.string, L"float") == 0 ||
            wcscmp(type1->term.string, L"number") == 0)
        ) {
            return term_init_atom(L"number");
        } else if(wcscmp(type2->term.string, L"float") == 0 && (
            wcscmp(type1->term.string, L"int") == 0 ||
            wcscmp(type1->term.string, L"number") == 0)
        ) {
            return term_init_atom(L"number");
        } else {
            return term_init_atom(L"term");
        }
    } else if(term_is_list(type1) && term_is_list(type2)) {
        head = term_list_get_head(type1);
        if(wcscmp(head->term.string, term_list_get_head(type2)->term.string) != 0)
            return term_init_atom(L"term");
        type = term_list_empty();
        term_increase_references(head);
        term_list_add_element(type, head);
        type1 = term_list_get_tail(type1);
        type2 = term_list_get_tail(type2);
        while(term_is_list(type1) && term_is_list(type2)
           && !term_list_is_null(type1) && !term_list_is_null(type2)) {
            term_list_add_element(type, tc_join_types(
                term_list_get_head(type1),
                term_list_get_head(type2)));
            type1 = term_list_get_tail(type1);
            type2 = term_list_get_tail(type2);
        }
        return type;
    } else {
        return term_init_atom(L"term");
    }
}

/**
  * 
  * This function returns a pointer to a term
  * containing the type of a given term.
  * 
  **/
Term *tc_get_type_term(Term *term) {
    Term *type_list, *type_head, *type;
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
        if(term_list_is_null(term)) {
            type = term_list_empty();
            term_list_add_element(type, term_init_atom(L"list"));
            term_list_add_element(type, term_init_atom(L"var"));
            return type;
        }
        type_list = NULL;
        while(term_is_list(term) && !term_list_is_null(term)) {
            type_head = tc_get_type_term(term_list_get_head(term));
            term = term_list_get_tail(term);
            if(type_list == NULL) {
                type_list = type_head;
            } else {
                type = tc_join_types(type_list, type_head);
                term_free(type_list);
                term_free(type_head);
                type_list = type;
            }
        }
        type = term_list_empty();
        term_list_add_element(type, term_init_atom(L"list"));
        term_list_add_element(type, type_list);
        return type;
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
  * containing normlized version of the type.
  * 
  * callable => (list _)
  * string => (list char)
  * term => _ (anonymous variable) | term
  * var => _ (anonymous variable)
  * int => (num numeral)
  * float => (num decimal)
  * number => (num _) | (num any)
  * (num any) => (num any) | (num _)
  * 
  **/
Term *tc_normalize_type(Term *type, int general) {
    Term *list;
    if(term_is_atom(type)) {
        if(general && wcscmp(L"term", type->term.string) == 0) {
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
            if(general)
                term_list_add_element(list, term_init_variable(L"_"));
            else
                term_list_add_element(list, term_init_atom(L"any"));
            return list;
        }
    } else if(term_is_list(type)) {
        list = term_list_empty();
        while(!term_list_is_null(type)) {
            term_list_add_element(list, tc_normalize_type(term_list_get_head(type), general));
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
    norm_type_expr = tc_normalize_type(type_expr, 0);
    norm_type = tc_normalize_type(type, 1);
    subs = semantics_unify_terms(norm_type, norm_type_expr, 1);
    term_free(type_expr);
    term_free(norm_type_expr);
    term_free(norm_type);
    return subs;
}