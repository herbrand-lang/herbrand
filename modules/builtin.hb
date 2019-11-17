(module builtin)



; LANGUAGE

;; typedef 2
;; (typedef +term +term)
;;
;; Type synonym.
;; (Synonim Type) is true if Synonim is a type synonym for Type.
(predicate typedef 2 (term term)
    (string (list char)))



; ARITHMETIC COMPARISON

;; (:==)/2
;; (:== @evaluable @evaluable)
;;
;; Arithmetic equal.
;; True if both numbers are equal.
#builtin #semidet
(predicate :== 2 (term term))

;; (:>=)/2
;; (:>= @evaluable @evaluable)
;;
;; Arithmetic greater than or equal to.
;; True if the first number is greater than or equal to the second one.
#builtin #semidet
(predicate :>= 2 (term term))

;; (:>)/2
;; (:> @evaluable @evaluable)
;;
;; Arithmetic greater than.
;; True if the first number is greater than the second one.
#builtin #semidet
(predicate :> 2 (term term))

;; (:<=)/2
;; (:<= @evaluable @evaluable)
;;
;; Arithmetic less than or equal to.
;; True if the first number is less than or equal to the second one.
#builtin #semidet
(predicate :<= 2 (term term))

;; (:<)/2
;; (:< @evaluable @evaluable)
;;
;; Arithmetic less than.
;; True if the first number is less than the second one.
#builtin #semidet
(predicate :< 2 (term term))

;; (:/==)/2
;; (:/== @evaluable @evaluable)
;;
;; Arithmetic not equal.
;; True if the compared numbers are not equal.
#builtin #semidet
(predicate :/== 2 (term term))



; ARITHMETIC EVALUATION

;; is/2
;; (is ?term @evaluable)
;;
;; Evaluate expression.
;; (is Result Expression) is true if and only if evaluating Expression as an expression gives Result as a result.
#builtin #semidet
(predicate is 2 (number term))

;; succ/2
;; (succ +integer -integer)
;; (succ -integer +integer)
;;
;; Return the successor of a positive number.
;; (succ Int Succ) is true if and only of Int is greater than or equal to 0 and Succ is Int + 1.
#builtin #semidet
(predicate succ 2 (int int))



; ATOM PROCESSING

;; atom_chars/2
;; (atom_chars +atom ?string)
;; (atom_chars -atom +string)
;;
;; Express an atom as a list of characters.
;; (atom_chars Atom List) succeeds if and only if List is made up of characters that in order form Atom.
#builtin #semidet
(predicate atom_chars 2 (atom string))

;; atom_concat/3
;; (atom_concat ?atom ?atom +atom)
;; (atom_concat +atom +atom -atom)
;;
;; Concatenate atoms.
;; (atom_concat Start End Whole) is true if and only if Whole is the atom obtained by adding the characters of
;; End at the end of Start. If Whole is the only argument instantiated, atom_concat/3 will obtain all possible
;; decompositions of it.
#builtin #semidet
(predicate atom_concat 3 (atom atom atom))

;; atom_length/2
;; (atom_length +atom ?integer)
;;
;; Length of an atom.
;; (atom_length Atom Length) is true if and only if the number of characters in the name of Atom is equal to
;; Length. If Length is not instantiated, atom_length/2 will calculate the length of Atom's name.
#builtin #semidet
(predicate atom_length 2 (atom int))

;; char_code/2
;; (char_code +character ?character_code)
;; (char_code -character +character_code)
;;
;; Character code of a character.
;; (char_code Char Code) succeeds if and only if Code is the character code of Char.
#builtin #semidet
(predicate char_code 2 (char int))



; CLAUSE MANIPULATION

;; asserta/1
;; (asserta @clause)
;;
;; Add term to the Herbrand database.
;; (asserta Clause) is true. It has for side effect the addition of the clause Clause to the database, placing
;; it before any other facts or rules with the same functor.
#builtin #det
(predicate asserta 1 (term))

;; assertz/1
;; (assertz @clause)
;;
;; Add term to the Herbrand database.
;; (assertz Clause) is true. Same side effect as asserta/1, but placing Clause after all the facts and rules
;; with the same functor.
#builtin #det
(predicate assertz 1 (term))

;; retract/1
;; (retract +clause)
;;
;; Delete term from Herbrand database.
;; (retract Clause) is true if there's at least one predicate in the database with the clause Clause. It has
;; for side effect the removal of that predicate.
#builtin #semidet
(predicate retract 1 (term))

;; retractall/1
;; (retractall +clause)
;;
;; Removes rules and clauses which unify with a term.
;; (retractall Head) removes all the clauses and rules whose heads unify with Head.
#builtin #det
(predicate retractall 1 (term))



; CONTROL CONSTRUCTS

;; and/*
;; (and ... +callable_term)
;;
;; Conjunction.
;; (and Goals) is true if and only if every goal in Goals is true.
#builtin #nondet
(predicate and 1 ((list callable)))

;; call/*
;; (call +callable_term ... +term)
;;
;; Invoke a callable term as a goal.
;; (call (Goal | Args)) is true if and only if Goal represents a goal which is true after appending Args to
;; its list of arguments. 
#builtin #nondet
(predicate call 1 ((list callable)))

;; catch/3
;; (catch +callable_term ?term +callable_term)
;;
;; Enable recovery from exceptions.
;; (catch Goal Catcher Handler) behaves as call/1 if no exception is raised when executing Goal. If an
;; exception is raised using throw/1 while Goal executes, and the Goal is the innermost goal for which
;; Catcher unifies with the argument of throw/1, all choice points generated by Goal are cut, the system
;; backtracks to the start of catch/3 while preserving the thrown exception term, and Handler is called
;; as in call/1.
#builtin #nondet
(predicate catch 3 (callable term callable))

;; (!)/0
;; (!)
;;
;; Cut.
;; (!) is true. All choice points between the cut and the parent goal are removed. The effect is commit
;; to use of both the current clause and the substitutions found at the point of the cut.
#builtin #det
(predicate ! 0 ())

;; false/0
;; (false)
;;
;; Alwais fail.
;; (false) is always false.
#builtin #semidet
(predicate false 0 ())

;; findall/3
;; (findall ?term +callable_term ?list)
;;
;; Find all the values that would make a goal succeed.
;; (findall Template Goal Instances) is true if and only if Instances is a list of values in the form
;; Templates that would make the goal Goal succeed. Usually, Template and Goal share some variables,
;; so Instances is filled with the values that make Goal succeed. If there is not a single value that
;; make Goal unify, Instances will be an empty list.
#builtin #semidet
(predicate findall 3 (term callable (list term)))

;; halt/1
;; (halt +number)
;;
;; Terminate a Herbrand processor and return message.
;; (halt X) exits the processor and returns to the system that invoked the processor, passing a message
;; through the X variable.
#builtin #det
(predicate halt 1 (int))

;; ite/3
;; (ite :callable_term :callable_term :callable_term)
;;
;; If-Then-Else.
;; (ite If Then Else) is true if and only if If is true and Then is true for the first solution of If,
;; or if If is false and Else is true for the first value with which If fails.
#builtin #nondet
(predicate ite 3 (callable callable callable))

;; not/1
;; (not @callable_term)
;;
;; Not provable.
;; (not Term) is true if and only if (call Term) is false.
#builtin #semidet
(predicate not 1 (callable))

;; once/1
;; (once @callable_term)
;;
;; Evaluate a term just once.
;; (once Term) is true. once/1 makes sure that Term fails or succeeds just once.
#builtin #semidet
(predicate once 1 (callable))

;; or/*
;; (or ... +callable_term)
;;
;; Disjunction.
;; (or Goals) is true if and only if any goal in Goals is true.
#builtin #nondet
(predicate or 1 ((list callable)))

;; repeat/0
;; (repeat)
;;
;; Provide infinite choice points.
;; (repeat) is true. It provides infinite choice points, what makes it perfect for creating loops.
#builtin #nondet
(predicate repeat 0 ())

;; throw/1
;; (throw +term)
;;
;; Raise an exception.
;; (throw Exception) raise the Exception exception. The system looks for the innermost catch/3
;; ancestor for which Exception unifies with the Catcher argument of the catch/3 call.
#builtin #nondet
(predicate throw 1 (term))

;; true/0
;; (true)
;;
;; Alwais succeed.
;; (true) is always true.
#builtin #det
(predicate true 0 ())



; HERBRAND FLAGS

;; current_herbrand_flag/2
;; (current_herbrand_flag ?flag ?term)
;;
;; Check Herbrand flag and flag values.
;; (current_herbrand_flag Flag Value) is true if and only if Flag is a valid Herbrand flag and Value
;; is its value.
#builtin #nondet
(predicate current_herbrand_flag 2 (atom term))

;; set_herbrand_flag/2
;; (set_herbrand_flag +flag @nonvar)
;;
;; Set value of flag.
;; (set_herbrand_flag Flag Value) sets the Herbrand flag Flag value to Value. This can either succeed
;; or raise an exception.
#builtin #det
(predicate set_herbrand_flag 2 (atom term))



; LOADING SOURCE FILES

;; consult/1
;; (consult +string)
;;
;; Read a file as a Herbrand source file.
;; (consult Path) is true when Path is a valid Herbrand source file.
#builtin #nondet
(predicate consult 1 (string))

;; import/1
;; (import +atom)
;;
;; Load a Herbrand module.
;; (import Module) is true when Module is a valid Herbrand module.
#builtin #nondet
(predicate import 1 (atom))



; TERM COMPARISON

;; (==)/2
;; (== @term @term)
;;
;; Term identical.
;; True if the compared terms are identical.
#builtin #semidet
(predicate == 2 (term term))

;; (>=)/2
;; (>= @term @term)
;;
;; Term greater than or equal to.
;; True if the first term is greater than or equal to the second one.
#builtin #semidet
(predicate >= 2 (term term))

;; (>)/2
;; (> @term @term)
;;
;; Term greater than.
;; True if the first term is greater than the second one.
#builtin #semidet
(predicate > 2 (term term))

;; (<=)/2
;; (<= @term @term)
;;
;; Term less than or equal to.
;; True if the first term is less than or equal to the second one.
#builtin #semidet
(predicate <= 2 (term term))

;; (<)/2
;; (< @term @term)
;;
;; Term less than.
;; True if the first term is less than the second one.
#builtin #semidet
(predicate < 2 (term term))

;; (/==)/2
;; (/== @term @term)
;;
;; Term not identical.
;; True if the compared terms are not identical.
#builtin #semidet
(predicate /== 2 (term term))



; TERM UNIFICATION

;; (/=)/2
;; (/= ?term ?term)
;;
;; Not term unification.
;; (/= X Y) is true if and only if X and Y are not unifiable. True if the unification fails.
#builtin #semidet
(predicate /= 2 (term term))

;; (=)/2
;; (= ?term ?term)
;;
;; Term unification.
;; (= X Y) is true if and only if X and Y are unifiable. True if the unification succeeds.
#builtin #semidet
(predicate = 2 (term term))



; TYPE TESTING

;; typeof/2
;; (typeof @term ?term)
;;
;; Check the type of an expression.
;; (typeof X T) is true if T unifies with the type of X.
#builtin #semidet
(predicate typeof 2 (term term))

;; atom/1
;; (atom @term)
;;
;; Check if atom.
;; (atom X) is true if and only if X is an atom.
#builtin #semidet
(predicate atom 1 (term))

;; char/1
;; (char @term)
;;
;; Check if chararacter.
;; (char X) is true if and only if X is a character.
#builtin #semidet
(predicate char 1 (term))

;; float/1
;; (float @term)
;;
;; Check if float.
;; (float X) is true if and only if X is a float.
#builtin #semidet
(predicate float 1 (term))

;; ground/1
;; (ground @term)
;;
;; Check if ground term.
;; (ground Term) is true if and only if Term holds no free variables.
#builtin #semidet
(predicate ground 1 (term))

;; float/1
;; (float @term)
;;
;; Check if float.
;; (float X) is true if and only if X is a float.
#builtin #semidet
(predicate float 1 (term))

;; list/1
;; (list @term)
;;
;; Check if list.
;; (list X) is true if and only if X is a list.
#builtin #semidet
(predicate list 1 (term))

;; nonvar/1
;; (nonvar @term)
;;
;; Check if not variable.
;; (nonvar X) is true if and only if X is not a variable.
#builtin #semidet
(predicate nonvar 1 (term))

;; number/1
;; (number @term)
;;
;; Check if number.
;; (number X) is true if and only if X is a number.
#builtin #semidet
(predicate number 1 (term))

;; string/1
;; (string @term)
;;
;; Check if string.
;; (string X) is true if and only if X is a string.
#builtin #semidet
(predicate string 1 (term))

;; var/1
;; (var @term)
;;
;; Check if variable.
;; (var X) is true if and only if X is a variable.
#builtin #semidet
(predicate var 1 (term))
