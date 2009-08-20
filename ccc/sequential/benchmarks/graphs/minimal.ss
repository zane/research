;;; ==== minimal.ss ====

(module minimal scheme

        (require "../../contract.ss")
        (require "./util.ss")
        (require "./ptfold.ss")
        (require "./minimal-aux.ss")

; A directed graph is stored as a connection matrix (vector-of-vectors)
; where the first index is the `from' vertex and the second is the `to'
; vertex.  Each entry is a bool indicating if the edge exists.
; The diagonal of the matrix is never examined.
; Make-minimal? returns a procedure which tests if a labelling
; of the verticies is such that the matrix is minimal.
; If it is, then the procedure returns the result of folding over
; the elements of the automoriphism group.  If not, it returns #f.
; The folding is done by calling folder via
;	(folder perm state accross)
; If the folder wants to continue, it should call accross via
;	(accross new-state)
; If it just wants the entire minimal? procedure to return something,
; it should return that.
; The ordering used is lexicographic (with #t > #f) and entries
; are examined in the following order:
;	1->0, 0->1
;
;	2->0, 0->2
;	2->1, 1->2
;
;	3->0, 0->3
;	3->1, 1->3
;	3->2, 2->3
;	...

(define matrix?
  (lambda (vec)
    (let ((size (vector-length vec)))
      (letrec 
        ((loop
           (lambda (i)
             (if (< i size )
               (if (and
                     (vector? (vector-ref vec i))
                     (= (vector-length (vector-ref vec i)) size))
                 (loop (+ i 1))
                 false)
               true))))
        (loop 0)))))


(provide make-minimal?)

;;(provide/contract 
;;  [make-minimal? (-> integer?-and-exact?-and-positive? 
;;                     (-> integer?-and-exact?-and-positive? 
;;                         matrix?
;;                         procedure?
;;                         any/c
;;                         any))])

(define make-minimal?
    (lambda (max-size)
 (let ((iotas
		    (proc->vector (+ max-size 1)
			giota))
		(perm
		    (make-vector max-size 0)))
	    (lambda (size graph folder state)
	    (fold-over-perm-tree (vector-ref iotas size)
		    (lambda (perm-x x state deeper accross)
			   (case (cmp-next-vertex graph perm x perm-x)
			    ((less)
				#f)
			    ((equal)
				(vector-set! perm x perm-x)
				(deeper (+ x 1)
				    state))
			    ((more)
				(accross state))))
		    0
        (t-folder-for-make-minimal size perm folder) ;imported from minima-aux
		    state)))))

; Given a graph, a partial permutation vector, the next input and the next
; output, return 'less, 'equal or 'more depending on the lexicographic
; comparison between the permuted and un-permuted graph.

(provide cmp-next-vertex)

;;(provide/contract 
;;  [cmp-next-vertex (->  matrix?
;;                        vector?
;;                        integer?-and-exact?-and-positive?
;;                        integer?-and-exact?-and-positive?
;;                        symbol?)])

(define cmp-next-vertex
    (lambda (graph perm x perm-x)
	(let ((from-x
		    (vector-ref graph x))
		(from-perm-x
		    (vector-ref graph perm-x)))
	    (let _-*-
		((y
			0))
		(if (= x y)
		    'equal
		    (let ((x->y?
				(vector-ref from-x y))
			    (perm-y
				(vector-ref perm y)))
			(cond ((eq? x->y?
				    (vector-ref from-perm-x perm-y))
				(let ((y->x?
					    (vector-ref (vector-ref graph y)
						x)))
				    (cond ((eq? y->x?
						(vector-ref (vector-ref graph perm-y)
						    perm-x))
					    (_-*- (+ y 1)))
					(y->x?
					    'less)
					(else
					    'more))))
			    (x->y?
				'less)
			    (else
				'more))))))))

)

