(module knight)

(predicate main 2 (int (list string))
	(Argc Argv
		(ite (/= Argc 3)
			(throw "wrong number of arguments")
			(
				(= Argv (_ Row Col))
				(tour (Row Col) T)
				(write stdout T)
			)
		)
	)
)

(predicate jump 2 (int int)
	(1 2) (2 1) (-1 2) (2 -1)
	(1 -2) (-2 1) (-1 -2) (-2 -1)
)

(predicate knight 3 (int int (list (list int)))
	(Row Col Tour
		(tour Row Col 0 () Tour)
	)
)

#local
(predicate tour 5 (int int int (list (list int)) (list (list int)))
	(_ _ 24 Tour Tour)
	(Row Col N Visited Tour
		(< N 24)
		(succ N M)
		(jump I J)
		(is X (+ I Row))
		(> X 0) (< X 6)
		(is Y (+ J Col))
		(> Y 0) (< Y 6)
		(nonmember (X Y) Visited)
		(tour X Y M ((Row Col)|Visited) Tour)
	)
)