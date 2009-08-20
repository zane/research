(module qslave scheme
        (provide slave-start)
        (provide enqueue)
        (provide is-queue-mt?)
        (provide print-queue)


        (require "./queue.scm")

        (define master? (make-parameter #t))

        (define (slave-start)
          (place slave-loop))

        (define slave-loop
          (lambda ()
            (parameterize ((master? #f))
            (begin
              (let* ((thunk (peek-queue)))
                (cond ((symbol? thunk) (slave-loop))
                      (else
                        (begin
                          (thunk)
                          (dequeue)
                          (slave-loop))))))))) 
 )
