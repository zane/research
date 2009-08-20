(module tables scheme

        (require "../../contract.ss")
        (require scheme/mpair)

        ;; TWO DIMENSIONAL TABLES
 
        (provide make-empty-table
                 lookup
                 insert!)

        
 (define (make-empty-table) (mlist 'TABLE))


(define two-dimensions? 
  (lambda (table)
    (or (not (or (null? table) (null? (mcdr table))))
        (and (mlist? table) (symbol=? (mcar table) 'TABLE)))))


  
(define (lookup table x y)
  (let ((one (massq x (mcdr table))))
    (if one
      (let ((two (massq y (mcdr one))))
        (if two (mcdr two) #f))
      #f)))


(define (insert! table x y value)
  (define (make-singleton-table x y)
    (mlist (mcons x y)))
  (let ((one (massq x (mcdr table))))
    (if one
	(set-mcdr! one (mcons (mcons y value) (mcdr one)))
	(set-mcdr! table (mcons (mcons x (make-singleton-table y value))
			      (mcdr table))))))

)
