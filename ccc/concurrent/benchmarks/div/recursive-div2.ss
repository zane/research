(module recursive-div2 scheme
        
  
        
    (require "../../contract.ss")

    (define even-length
          (lambda (l)
            (cond ((null? l) #t)
                  ((null? (cdr l)) #f)
                  (else (even-length (cddr l))))))


    (provide/contract (recursive-div2 (->   even-length any)))

    (define (recursive-div2 l)
      (cond ((null? l) '())
            (else (cons (car l) (recursive-div2 (cddr l))))))
)
