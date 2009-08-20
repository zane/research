(module smin scheme

        (require "../../contract.ss")

        (provide is-sorted)

        (define (is-sorted l op)
          (cond ((empty? l) #t)
                ((empty? (cdr l)) #t)
                (else (if (op (car l) (cadr l))
                        (is-sorted (cdr l) op)
                        #f))))

        (provide smin)

       ;; (provide/contract 
       ;;   (smin (->  (lambda (x) 
       ;;                         (and 
       ;;                           (andmap (lambda (x)
       ;;                                      (and (number? x)
       ;;                                          (> x 0)))
       ;;                                   x)
       ;;                          (is-sorted x <))) 
       ;;              any)))

        (define (smin l)
          (cond ((empty? l) 'empty-list)
                (else (car l))))
)
