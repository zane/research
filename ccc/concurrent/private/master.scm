(module master scheme
        (provide master-start)
        (provide enqueue)

        (require "./slave.scm")


        (define (master-start thunk)
          (slave-start)
          (let ((result (thunk)))
            (if (is-queue-mt?)
              result
              (void))))
) 
