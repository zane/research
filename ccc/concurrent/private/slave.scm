(module slave scheme
        (provide slave-start)
        (provide enqueue)
        (provide is-queue-mt?)


        (require "./middle-man.scm")


        (define (slave-start)
          (thread middle-man-loop)
          (place slave-loop))

        (define slave-loop
          (lambda () 
            (let* ((thunk (peek-queue)))
              (thunk)
              (dequeue)
              (slave-loop))))

) 
