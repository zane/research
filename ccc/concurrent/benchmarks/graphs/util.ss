
;;; ==== util.ss ====

(module util scheme

        (require "../../contract.ss")
     
       (provide integer?-and-exact?-and-positive?)

        ; Fold over list elements, associating to the left.

(provide/contract 
  [fold (-> list? procedure? any/c any/c)])

(define fold
   (lambda (lst folder state)
     (do ((lst lst (cdr lst))
          (state state (folder (car lst) state)))
       ((null? lst)	state))))

; Given the size of a vector and a procedure which
; sends indicies to desired vector elements, create
; and return the vector.


(define integer?-and-exact?-and-positive?
  (lambda (size)
    (and (integer? size)
         (exact? size)
         (>= size 0))))

(define (n-length-vector n)
  (lambda (vec)
    (and 
      (vector? vec)
      (= n (vector-length vec)))))

(provide/contract 
  [proc->vector (->d 
                  ([size integer?-and-exact?-and-positive?]
                   [f procedure?])
                  ()
                  [result (n-length-vector size)])])

(define proc->vector
  (lambda (size f)
   (if (zero? size)
        (vector)
        (let ((x (make-vector size (f 0))))
          (let loop ((i 1))
            (if (< i size) 
              (begin               ; [wdc - was when]
                (vector-set! x i (f i))
                (loop (+ i 1)))
              '()))
          x))))


(define (equal-length-vectors before)
  (lambda (after)
    (= (vector-length before) (vector-length after))))

(provide/contract [vector-fold (-> vector?
                                   (future/c procedure?)
                                   any/c 
                                   any/c)])


(define vector-fold
    (lambda (vec folder state)
     (let ((len
		    (vector-length vec)))
	    (do ((i 0
			(+ i 1))
		    (state state
			(folder (vector-ref vec i)
			    state)))
		((= i len)
		    state)))))

(provide/contract 
  [vec-map (->d 
                  ([vec   (future/c vector?)]
                   [proc   procedure?])
                  ()
                  [result (future/c (equal-length-vectors vec))])])


(define vec-map
    (lambda (vec proc)
	(proc->vector (vector-length vec)
	    (lambda (i)
		(proc (vector-ref vec i))))))

; Given limit, return the list 0, 1, ..., limit-1.


(define (n-length-list n)
  (lambda (l)
    (and 
      (list? l)
      (= n (length l)))))


(provide/contract 
  [giota (->d ([limit (future/c integer?-and-exact?-and-positive?)])
              ()
              [result (future/c (n-length-list limit))])])

(define giota
    (lambda (limit)
 (let _-*-
	    ((limit
		    limit)
		(res
		    '()))
	    (if (zero? limit)
		res
		(let ((limit
			    (- limit 1)))
		    (_-*- limit
			(cons limit res)))))))

; Fold over the integers [0, limit).

(provide/contract 
  [gnatural-fold (-> (future/c integer?-and-exact?-and-positive?)
                     (future/c procedure?)
                     any/c
                     any)])
    
(define gnatural-fold
    (lambda (limit folder state)
	  	(do ((i 0
		    (+ i 1))
	   	(state state
		    (folder i state)))
	      ((= i limit)
		  state))))

; Iterate over the integers [0, limit).

(provide/contract 
  [gnatural-for-each (-> (future/c integer?-and-exact?-and-positive?)
                         procedure?
                         any)])
 
(define gnatural-for-each
    (lambda (limit proc!)
 (do ((i 0
		    (+ i 1)))
	    ((= i limit))
	    (proc! i))))

(provide/contract 
  [natural-for-all? (->  integer?-and-exact?-and-positive?
                         (future/c procedure?)
                         any)])
 
(define natural-for-all?
    (lambda (limit ok?)
 (let _-*-
	    ((i 0))
	    (or (= i limit)
		(and (ok? i)
		    (_-*- (+ i 1)))))))

(provide/contract 
  [natural-there-exists? (-> (future/c integer?-and-exact?-and-positive?)
                             procedure?
                             any)])

(define natural-there-exists?
    (lambda (limit ok?)
  (let _-*-
	    ((i 0))
	    (and (not (= i limit))
		(or (ok? i)
		    (_-*- (+ i 1)))))))

(provide/contract 
  [there-exists? (-> (future/c list?) (future/c procedure?)  any)])

(define there-exists?
    (lambda (lst ok?)
	'(assert (list? lst)
	    lst)
	'(assert (procedure? ok?)
	    ok?)
	(let _-*-
	    ((lst lst))
	    (and (not (null? lst))
		(or (ok? (car lst))
		    (_-*- (cdr lst)))))))

)

