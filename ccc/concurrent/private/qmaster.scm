(module qmaster scheme
        (provide master-start)
        (provide master?)
        (provide enqueue)
        (provide slave-start)

        (require "./qslave.scm")

        (define master? (make-parameter #t))  
     

        (define (master-start thunk)
          (let ((result (thunk)))
            (master-loop result)))

        (define (master-loop result)
          (if (is-queue-mt?)
            result
            (begin
              (master-loop result))))
) 
