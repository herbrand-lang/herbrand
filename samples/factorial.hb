(predicate fac 2 (int int)
	(0 1)
	(N X
		(> N 0)
		(succ M N)
		(fac M Y)
		(is X (* N Y))
	)
)

#tailrec
(predicate tailfac 3 (int int int)
	(0 X X)
	(N X Y
		(> N 0)
		(succ M N)
		(is Z (* N X))
		(tailfac M Z Y)
	)
)
