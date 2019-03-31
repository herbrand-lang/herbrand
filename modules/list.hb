(module list)

(predicate append 3 ((list _) (list _) (list _))
	(() X X)
	((H|T) X (H|S)
		(append T X S)))

(predicate powerset 2 ((list _) (list _))
	(() ())
	((_|T) P
		(powerset T P))
	((H|T) (H|P)
		(powerset T P)))

(predicate permutation 2 ((list _) (list _))
	(() ())
	((H|T) P
		(permutation T Q)
		(append A B Q)
		(append A (H|B) P)))

(predicate member 2 ((list _) (list _))
	(H (H|_))
	(X (_|T)
		(member X T)))

(predicate nth 3 (int (list _) _)
	(0 (H|_) H)
	(N (_|T) X
		(> N 0)
		(succ M N)
		(nth M T X)))
