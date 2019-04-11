(module list)

#semidet
(predicate head 2 ((list _) _)
	((H|_) H))

#semidet
(predicate tail 2 ((list _) (list _))
	((_|T) T))

#det
(predicate length 2 ((list _) int)
	(X N (length_aux X 0 N)))

#local #det
(predicate length_aux 3 ((list _) int int)
	(() N N)
	((_|T) A N
		(succ A B)
		(length_aux T B N)))

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

#semidet
(predicate nonmember 2 ((list _) (list _))
	(_ ())
	(X (Y|T)
		(/== X Y)
		(nonmember X T)))

(predicate nth 3 (int (list _) _)
	(0 (H|_) H)
	(N (_|T) X
		(succ M N)
		(nth M T X)))

(predicate map 2 (_ (list _))
	(_ ())
	(P (X|Xs)
		(call P X)
		(map P Xs)))

(predicate map2 3 (_ (list _) (list _))
	(_ () ())
	(P (X|Xs) (Y|Ys)
		(call P X Y)
		(map2 P Xs Ys)))

(predicate map3 4 (_ (list _) (list _) (list _))
	(_ () () ())
	(P (X|Xs) (Y|Ys) (Z|Zs)
		(call P X Y Z)
		(map3 P Xs Ys Zs)))

(predicate map4 5 (_ (list _) (list _) (list _) (list _))
	(_ () () () ())
	(P (X|Xs) (Y|Ys) (Z|Zs) (W|Ws)
		(call P X Y Z W)
		(map4 P Xs Ys Zs Ws)))

(predicate map5 6 (_ (list _) (list _) (list _) (list _) (list _))
	(_ () () () () ())
	(P (X|Xs) (Y|Ys) (Z|Zs) (W|Ws) (V|Vs)
		(call P X Y Z W V)
		(map5 P Xs Ys Zs Ws Vs)))

(predicate map6 7 (_ (list _) (list _) (list _) (list _) (list _) (list _))
	(_ () () () () () ())
	(P (X|Xs) (Y|Ys) (Z|Zs) (W|Ws) (V|Vs) (U|Us)
		(call P X Y Z W V U)
		(map6 P Xs Ys Zs Ws Vs Us)))

(predicate map7 8 (_ (list _) (list _) (list _) (list _) (list _) (list _) (list _))
	(_ () () () () () () ())
	(P (X|Xs) (Y|Ys) (Z|Zs) (W|Ws) (V|Vs) (U|Us) (T|Ts)
		(call P X Y Z W V U T)
		(map7 P Xs Ys Zs Ws Vs Us Ts)))