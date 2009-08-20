;;; ==== ptfold.ss ====

(module ptfold scheme

        (require "../../contract.ss")
        (require "./util.ss")

; Fold over the tree of permutations of a universe.
; Each branch (from the root) is a permutation of universe.
; Each node at depth d corresponds to all permutations which pick the
; elements spelled out on the branch from the root to that node as
; the first d elements.
; Their are two components to the state:
;	The b-state is only a function of the branch from the root.
;	The t-state is a function of all nodes seen so far.
; At each node, b-folder is called via
;	(b-folder elem b-state t-state deeper accross)
; where elem is the next element of the universe picked.
; If b-folder can determine the result of the total tree fold at this stage,
; it should simply return the result.
; If b-folder can determine the result of folding over the sub-tree
; rooted at the resulting node, it should call accross via
;	(accross new-t-state)
; where new-t-state is that result.
; Otherwise, b-folder should call deeper via
;	(deeper new-b-state new-t-state)
; where new-b-state is the b-state for the new node and new-t-state is
; the new folded t-state.
; At the leaves of the tree, t-folder is called via
;	(t-folder b-state t-state accross)
; If t-folder can determine the result of the total tree fold at this stage,
; it should simply return that result.
; If not, it should call accross via
;	(accross new-t-state)
; Note, fold-over-perm-tree always calls b-folder in depth-first order.
; I.e., when b-folder is called at depth d, the branch leading to that
; node is the most recent calls to b-folder at all the depths less than d.
; This is a gross efficiency hack so that b-folder can use mutation to
; keep the current branch.

(provide/contract 
  [fold-over-perm-tree
    (-> (future/c list?) procedure? any/c  procedure? any/c any)])


(define fold-over-perm-tree
    (lambda (universe b-folder b-state t-folder t-state)
   (let _-*-
	    ((universe
		    universe)
		(b-state
		    b-state)
		(t-state
		    t-state)
		(accross
		    (lambda (final-t-state)
			final-t-state)))
	    (if (null? universe)
		(t-folder b-state t-state accross)
		(let _-**-
		    ((in
			    universe)
			(out
			    '())
			(t-state
			    t-state))
		    (let* ((first
				(car in))
			    (rest
				(cdr in))
			    (accross
				(if (null? rest)
				    accross
				    (lambda (new-t-state)
					(_-**- rest
					    (cons first out)
					    new-t-state)))))
			(b-folder first
			    b-state
			    t-state
			    (lambda (new-b-state new-t-state)
				(_-*- (fold out cons rest)
				    new-b-state
				    new-t-state
				    accross))
			    accross)))))))


)
