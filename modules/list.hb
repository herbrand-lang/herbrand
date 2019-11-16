(module list)



; HIGHER ORDER

;; filter/3
;; (filter :callable_term +list ?list)
;;
;; Filter elements for which a goal succeeds.
;; (filter Goal List FilterList) is true if and only if FilterList is a list containing only the elements
;; of List that satisfy Goal.
(predicate filter 3 (term (list term) (list term))
	(_ () ())
	(P (X|Xs) Ys
		(ite (call P X) (= Ys (X|Zs)) (= Ys Zs))
		(filter P Xs Zs)))

;; foldl/4
;; (foldl :callable_term +list +term -term)
;;
;; Reduce list to a single value.
;; (foldl Goal Start List End) is true if and only if End is the result of applying Goal to the elements
;; of List, from left to right, in a cumulative way, using Start as initial accumulator.
(predicate foldl 4 (term term (list term) (list term))
	(_ Acc () Acc)
	(P Acc (X|Xs) Acc3
		(call P Acc X Acc2)
		(foldl P Acc2 Xs Acc3)))

;; foldr/4
;; (foldr :callable_term +list +term -term)
;;
;; Reduce list to a single value.
;; (foldr Goal Start List End) is true if and only if End is the result of applying Goal to the elements
;; of List, from right to left, in a cumulative way, using Start as initial accumulator.
(predicate foldr 4 (term term (list term) (list term))
	(_ Acc () Acc)
	(P Acc (X|Xs) Acc3
		(foldr P Acc Xs Acc2)
		(call P X Acc2 Acc3)))

;; map/2
;; (map :callable_term ?list)
;;
;; Check if goal can be applied to a list.
;; (map Goal List) is true if and only if Goal can be successfully applied to List. If Goal fails for any
;; of List's elements, map/2 fails.
(predicate map 2 (term (list term))
	(_ ())
	(P (X|Xs)
		(call P X)
		(map P Xs)))

;; map2/3
;; (map2 :callable_term ?list ?list)
;;
;; Check if goal can be applied to several lists.
;; As in map/2, but operating two elements at a time from the two lists. In this case, the Goal will consume
;; two elements each time, one from each list.
(predicate map2 3 (term (list term) (list term))
	(_ () ())
	(P (X|Xs) (Y|Ys)
		(call P X Y)
		(map2 P Xs Ys)))

;; map3/4
;; (map3 :callable_term ?list ?list ?list)
;;
;; Check if goal can be applied to several lists.
;; As in map/2, but operating three elements at a time from the three lists. In this case, the Goal will consume
;; three elements each time, one from each list.
(predicate map3 4 (term (list term) (list term) (list term))
	(_ () () ())
	(P (X|Xs) (Y|Ys) (Z|Zs)
		(call P X Y Z)
		(map3 P Xs Ys Zs)))

;; map4/5
;; (map4 :callable_term ?list ?list ?list ?list)
;;
;; Check if goal can be applied to several lists.
;; As in map/2, but operating four elements at a time from the four lists. In this case, the Goal will consume
;; four elements each time, one from each list.
(predicate map4 5 (term (list term) (list term) (list term) (list term))
	(_ () () () ())
	(P (X|Xs) (Y|Ys) (Z|Zs) (W|Ws)
		(call P X Y Z W)
		(map4 P Xs Ys Zs Ws)))

;; map5/6
;; (map5 :callable_term ?list ?list ?list ?list ?list)
;;
;; Check if goal can be applied to several lists.
;; As in map/2, but operating five elements at a time from the five lists. In this case, the Goal will consume
;; five elements each time, one from each list.
(predicate map5 6 (term (list term) (list term) (list term) (list term) (list term))
	(_ () () () () ())
	(P (X|Xs) (Y|Ys) (Z|Zs) (W|Ws) (V|Vs)
		(call P X Y Z W V)
		(map5 P Xs Ys Zs Ws Vs)))

;; map6/7
;; (map6 :callable_term ?list ?list ?list ?list ?list ?list)
;;
;; Check if goal can be applied to several lists.
;; As in map/2, but operating six elements at a time from the six lists. In this case, the Goal will consume
;; six elements each time, one from each list.
(predicate map6 7 (term (list term) (list term) (list term) (list term) (list term) (list term))
	(_ () () () () () ())
	(P (X|Xs) (Y|Ys) (Z|Zs) (W|Ws) (V|Vs) (U|Us)
		(call P X Y Z W V U)
		(map6 P Xs Ys Zs Ws Vs Us)))

;; map7/8
;; (map7 :callable_term ?list ?list ?list ?list ?list ?list ?list)
;;
;; Check if goal can be applied to several lists.
;; As in map/2, but operating seven elements at a time from the seven lists. In this case, the Goal will consume
;; seven elements each time, one from each list.
(predicate map7 8 (term (list term) (list term) (list term) (list term) (list term) (list term) (list term))
	(_ () () () () () () ())
	(P (X|Xs) (Y|Ys) (Z|Zs) (W|Ws) (V|Vs) (U|Us) (T|Ts)
		(call P X Y Z W V U T)
		(map7 P Xs Ys Zs Ws Vs Us Ts)))



; LIST MANIPULATION

;; append/3
;; (append ?list ?list ?list)
;;
;; Join two lists.
;; (append L1 L2 L1_L2) is true and only true if L1_L2 is a list made up by concatenating L1 and L2.
#nondet
(predicate append 3 ((list term) (list term) (list term))
	(() X X)
	((H|T) X (H|S)
		(append T X S)))

;; head/2
;; (head ?list ?term)
;;
;; First element of a list.
;; (head List X) is true if X is the first element in List.
#semidet
(predicate head 2 ((list term) term)
	((H|_) H))

;; length/2
;; (length ?list ?integer)
;;
;; Length of a list.
;; (length List Integer) is true if and only if Integer is equal to the number of elements in List.
#semidet
(predicate length 2 ((list term) int)
	(X N (length_aux X 0 N)))

#local #semidet
(predicate length_aux 3 ((list term) int int)
	(() N N)
	((_|T) A N
		(succ A B)
		(length_aux T B N)))

;; member/2
;; (member ?term ?list)
;;
;; Check membership of element in list.
;; (member X List) is true if and only if X is an element contained in List. If X is not instantiated, it will
;; be instantiated with all the values in List.
#nondet
(predicate member 2 ((list term) (list term))
	(H (H|_))
	(X (_|T)
		(member X T)))

;; nonmember/2
;; (nonmember +term ?list)
;;
;; Check non membership of element in list.
;; (nonmember X List) is true if and only if X is not an element contained in List.
#semidet
(predicate nonmember 2 ((list term) (list term))
	(_ ())
	(X (Y|T)
		(/== X Y)
		(nonmember X T)))

;; nth/3
;; (nth ?integer ?list ?term)
;;
;; Get the Nth element of a list.
;; (nth Index List Item) is true if and only if Item is the element of List on the Index-th position. nth/3
;; starts the index count at 0.
(predicate nth 3 (int (list term) term)
	(0 (H|_) H)
	(N (_|T) X
		(succ M N)
		(nth M T X)))

;; permutation/2
;; (permutation ?list ?list)
;;
;; Permutation of list.
;; (permutation List PermutateList) is true if and only if PermutateList is a permutation of List. If one of
;; those parameters is uninstantiated, permutation/2 will calculate all the possible permutations. It's important
;; to keep in mind that this predicate is computationally expensive, since a list of T length has T! permutations.
#nondet
(predicate permutation 2 ((list term) (list term))
	(() ())
	((H|T) P
		(permutation T Q)
		(append A B Q)
		(append A (H|B) P)))

;; powerset/2
;; (powerset ?list ?list)
;;
;; Powerset of a list.
;; (powerset List Powerset) is true if and only if Powerset is an element of the powerset of List. If one of those
;; parameters is uninstantiated, powerset/2 will calculate all the possible sets of the powerset.
#nondet
(predicate powerset 2 ((list term) (list term))
	(() ())
	((_|T) P
		(powerset T P))
	((H|T) (H|P)
		(powerset T P)))

;; tail/2
;; (tail ?list ?list)
;;
;; Tail of a list.
;; (tail List Tail) is true if Tail is the tail of List.
#semidet
(predicate tail 2 ((list term) (list term))
	((_|T) T))