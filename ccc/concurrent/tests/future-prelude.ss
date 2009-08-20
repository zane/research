(module future-prelude scheme

        (require "../contract.ss")

        (provide/contract (f (->
                               (->
                                 (future/c (or/c 
                                             (lambda (x) (= x  0))
                                             (lambda (x) (> x 10))))
                                 (future/c (lambda (x) true)))
                               (lambda (x) true))))
        (define (f g)
          (g 11))
)
