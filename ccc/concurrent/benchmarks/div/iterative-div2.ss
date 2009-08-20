(module iterative-div2 scheme
        
    (require "../../contract.ss")

    (define even-length
          (lambda (l)
            (cond ((null? l) #t)
                  ((null? (cdr l)) #f)
                  (else (even-length (cddr l))))))

      (provide/contract (iterative-div2 (-> (future/c even-length) any)))

    (define (iterative-div2 l)
      (do ((l l (cddr l))
           (a '() (cons (car l) a)))
        ((null? l) a)))

)
