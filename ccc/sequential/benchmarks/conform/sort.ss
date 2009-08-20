(module sort scheme

       (require "../../contract.ss")
       (require "./merge.ss")
       (require "./vector-copy.ss")
       (require scheme/mpair)
       
       (provide sort)
;; SORT

(define (equal-size arg)
 (lambda (res) 
  (cond ((vector? arg) (= (vector-length arg) (vector-length res)))
        (else   (= (mlength arg) (mlength res))))))


(define (sorted-list? pred res) 
  (cond ((null? res) #t)
        ((null? (mcdr res)) #t)
        (else (and 
                (pred (mcar res) (mcar (mcdr res)))
                (sorted-list? (mcdr res))))))

(define (sorted-vector? pred res)
    (let ((limit (vector-length res)))
    (letrec ((sorted-vector?-aux
              (lambda (res i)
                (if (< i (- limit 1))
                  (and 
                    (pred (vector-ref res i) (vector-ref res (+ i 1)))
                    (sorted-vector?-aux (+ i 1)))
                  #t))))
      (sorted-vector?-aux 0))))


(define (sorted pred)
  (lambda (res)
    (cond ((vector? res) (sorted-vector? pred res))
          (else (sorted-list? pred res)))))



                               

(define (sort obj pred)
  (cond ((or (mpair? obj) (null? obj))
         (sortf obj pred))
        ((vector? obj)
         (sort! (vector-copy obj) pred))))

;; This merge sort is stable for partial orders (for predicates like
;; <=, rather than like <).

 (define (sortf l pred)
    (if (and (mpair? l) (mpair? (mcdr l)))
      (split l '() '() pred)
      l))

 (define (split l one two pred)
    (if (mpair? l)
    	(split (mcdr l) two (mcons (mcar l) one) pred)
	    (merge (sortf one pred) (sortf two pred) pred)))


(define (sort! v pred)
  (define (sort-internal! vec temp low high)
    (when (< low high)
      (let* ((middle (quotient (+ low high) 2))
             (next (+ middle 1)))
        (sort-internal! temp vec low middle)
        (sort-internal! temp vec next high)
        (let loop ((p low) (p1 low) (p2 next))
          (when (not (> p high))
            (cond ((> p1 middle)
                   (vector-set! vec p (vector-ref temp p2))
                   (loop (+ p 1) p1 (+ p2 1)))
                  ((or (> p2 high)
                       (pred (vector-ref temp p1)
                             (vector-ref temp p2)))
                   (vector-set! vec p (vector-ref temp p1))
                   (loop (+ p 1) (+ p1 1) p2))
                  (else (vector-set! vec p (vector-ref temp p2))
                        (loop (+ p 1) p1 (+ p2 1)))))))))
  (sort-internal! v
		  (vector-copy v)
		  0
		  (- (vector-length v) 1))
  v)

)
