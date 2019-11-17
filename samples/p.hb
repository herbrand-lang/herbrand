(predicate p 1 _
	(a)
	(b)
	(X (q X))
	(e))

(predicate q 1 _
	(c (throw error))
	(X (r X) (!)))

(predicate r 1 _
	(c)
	(d))

(predicate append 3 ((list X) (list X) (list X))
	(() Ys Ys)
	((X|Xs) Ys (X|Zs)
		(append Xs Ys Zs)))